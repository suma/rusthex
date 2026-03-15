use memmap2::Mmap;
use std::cell::Cell;
use std::fs::File;
use std::io::{BufWriter, Write};
use std::path::PathBuf;

/// Data source for background search operations
/// Always uses InMemory after Piece Table migration
pub enum SearchDataSource {
    /// In-memory data (with all edits materialized)
    InMemory(Vec<u8>),
}

/// Source buffer for a piece
#[derive(Debug, Clone, Copy, PartialEq)]
enum PieceSource {
    /// Original file/data (read-only)
    Original,
    /// Add buffer (append-only)
    Add,
}

/// A piece in the piece table
#[derive(Debug, Clone)]
struct Piece {
    source: PieceSource,
    start: usize,
    length: usize,
}

/// Cache for accelerating sequential byte access
#[derive(Debug, Clone, Copy)]
struct PieceCache {
    piece_index: usize,
    logical_start: usize,
}

/// Original data buffer (read-only after loading)
enum OriginalBuffer {
    /// In-memory storage (for small files or new documents)
    InMemory(Vec<u8>),
    /// Memory-mapped file (for large files)
    MemoryMapped {
        #[allow(dead_code)]
        mmap: Mmap,
        #[allow(dead_code)]
        file: File,
    },
}

impl OriginalBuffer {
    fn get(&self, offset: usize) -> Option<u8> {
        match self {
            OriginalBuffer::InMemory(data) => data.get(offset).copied(),
            OriginalBuffer::MemoryMapped { mmap, .. } => mmap.get(offset).copied(),
        }
    }

    /// Get a slice of the backing data
    fn as_bytes(&self) -> &[u8] {
        match self {
            OriginalBuffer::InMemory(data) => data,
            OriginalBuffer::MemoryMapped { mmap, .. } => mmap,
        }
    }

    #[allow(dead_code)]
    fn len(&self) -> usize {
        match self {
            OriginalBuffer::InMemory(data) => data.len(),
            OriginalBuffer::MemoryMapped { mmap, .. } => mmap.len(),
        }
    }
}

/// Edit operation types for undo/redo
#[derive(Debug, Clone)]
#[allow(dead_code)]
enum EditOperation {
    Replace {
        offset: usize,
        old_bytes: Vec<u8>,
        new_bytes: Vec<u8>,
    },
    Insert {
        offset: usize,
        data: Vec<u8>,
    },
    Delete {
        offset: usize,
        deleted_bytes: Vec<u8>,
    },
}

/// Undo record storing the operation and piece table snapshot
#[derive(Debug, Clone)]
struct UndoRecord {
    operation: EditOperation,
    pieces_snapshot: Vec<Piece>,
    cached_length: usize,
}

/// Document manages binary data using a Piece Table for efficient insert/delete
pub struct Document {
    /// Original data (read-only)
    original: OriginalBuffer,
    /// Append-only buffer for new data
    add_buffer: Vec<u8>,
    /// Piece table
    pieces: Vec<Piece>,
    /// Cached document length (sum of all piece lengths)
    cached_length: usize,
    /// Cache for sequential access optimization
    access_cache: Cell<Option<PieceCache>>,

    /// File metadata
    file_path: Option<PathBuf>,
    /// Display name override (used for tabs without a file path)
    display_name: Option<String>,
    /// Change tracking
    has_unsaved_changes: bool,

    /// Edit history for undo
    undo_stack: Vec<UndoRecord>,
    /// Edit history for redo
    redo_stack: Vec<UndoRecord>,
    /// Maximum undo levels
    max_undo_levels: usize,
    /// Threshold for using memory-mapped files
    mmap_threshold: usize,
}

impl Document {
    /// Create a new empty document
    pub fn new() -> Self {
        Self {
            original: OriginalBuffer::InMemory(Vec::new()),
            add_buffer: Vec::new(),
            pieces: Vec::new(),
            cached_length: 0,
            access_cache: Cell::new(None),
            file_path: None,
            display_name: None,
            has_unsaved_changes: false,
            undo_stack: Vec::new(),
            redo_stack: Vec::new(),
            max_undo_levels: 1000,
            mmap_threshold: 10 * 1024 * 1024, // 10MB
        }
    }

    /// Create document with initial data
    pub fn with_data(data: Vec<u8>) -> Self {
        let len = data.len();
        let mut doc = Self::new();
        doc.original = OriginalBuffer::InMemory(data);
        if len > 0 {
            doc.pieces.push(Piece {
                source: PieceSource::Original,
                start: 0,
                length: len,
            });
            doc.cached_length = len;
        }
        doc
    }

    /// Load document from file
    pub fn from_file(path: PathBuf) -> std::io::Result<Self> {
        let mut doc = Self::new();
        doc.load(path)?;
        Ok(doc)
    }

    /// Load file, choosing backend based on file size
    pub fn load(&mut self, path: PathBuf) -> std::io::Result<()> {
        let file = File::open(&path)?;
        let metadata = file.metadata()?;
        let file_size = metadata.len() as usize;

        self.original = if file_size > self.mmap_threshold {
            let mmap = unsafe { Mmap::map(&file)? };
            OriginalBuffer::MemoryMapped { mmap, file }
        } else {
            let data = std::fs::read(&path)?;
            OriginalBuffer::InMemory(data)
        };

        // Reset piece table to single piece covering entire original
        self.pieces.clear();
        if file_size > 0 {
            self.pieces.push(Piece {
                source: PieceSource::Original,
                start: 0,
                length: file_size,
            });
        }
        self.cached_length = file_size;
        self.add_buffer.clear();
        self.access_cache.set(None);

        self.file_path = Some(path);
        self.undo_stack.clear();
        self.redo_stack.clear();
        self.has_unsaved_changes = false;

        Ok(())
    }

    /// Save document to current file path
    pub fn save(&mut self) -> std::io::Result<()> {
        if let Some(path) = &self.file_path {
            self.save_as(path.clone())
        } else {
            Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                "No file path set",
            ))
        }
    }

    /// Save document to specified path
    pub fn save_as(&mut self, path: PathBuf) -> std::io::Result<()> {
        {
            let file = File::create(&path)?;
            let mut writer = BufWriter::new(file);
            self.write_to(&mut writer)?;
            writer.flush()?;
        }

        self.file_path = Some(path.clone());
        self.load(path)?;

        Ok(())
    }

    /// Write document contents to a writer without materializing into Vec
    pub fn write_to<W: Write>(&self, writer: &mut W) -> std::io::Result<()> {
        for piece in &self.pieces {
            writer.write_all(self.piece_slice(piece))?;
        }
        Ok(())
    }

    /// Write a range of document contents to a writer without materializing into Vec
    pub fn write_range_to<W: Write>(
        &self,
        range: std::ops::Range<usize>,
        writer: &mut W,
    ) -> std::io::Result<()> {
        if range.is_empty() {
            return Ok(());
        }
        if range.end > self.cached_length {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                format!(
                    "Range {}..{} out of bounds (size: {})",
                    range.start, range.end, self.cached_length
                ),
            ));
        }

        let mut logical_pos = 0;
        for piece in &self.pieces {
            let piece_end = logical_pos + piece.length;

            if piece_end <= range.start {
                logical_pos = piece_end;
                continue;
            }
            if logical_pos >= range.end {
                break;
            }

            let slice = self.piece_slice(piece);
            let copy_start = range.start.saturating_sub(logical_pos);
            let copy_end = (range.end - logical_pos).min(piece.length);
            writer.write_all(&slice[copy_start..copy_end])?;

            logical_pos = piece_end;
        }
        Ok(())
    }

    // =====================================================
    // Piece Table core operations
    // =====================================================

    /// Get byte from original or add buffer based on piece source
    fn get_byte_from_source(&self, piece: &Piece, offset_in_piece: usize) -> Option<u8> {
        let source_offset = piece.start + offset_in_piece;
        match piece.source {
            PieceSource::Original => self.original.get(source_offset),
            PieceSource::Add => self.add_buffer.get(source_offset).copied(),
        }
    }

    /// Get the backing byte slice for a piece
    fn piece_slice(&self, piece: &Piece) -> &[u8] {
        let buf = match piece.source {
            PieceSource::Original => self.original.as_bytes(),
            PieceSource::Add => &self.add_buffer,
        };
        &buf[piece.start..piece.start + piece.length]
    }

    /// Find the piece containing the given logical offset.
    /// Returns (piece_index, offset_within_piece, logical_start_of_piece).
    fn find_piece(&self, offset: usize) -> Option<(usize, usize, usize)> {
        if offset >= self.cached_length {
            return None;
        }

        // Check cache for sequential access optimization
        if let Some(cache) = self.access_cache.get()
            && cache.piece_index < self.pieces.len()
        {
            let piece = &self.pieces[cache.piece_index];
            let piece_end = cache.logical_start + piece.length;
            if offset >= cache.logical_start && offset < piece_end {
                return Some((
                    cache.piece_index,
                    offset - cache.logical_start,
                    cache.logical_start,
                ));
            }
            // Check next piece (common for sequential access)
            let next_idx = cache.piece_index + 1;
            if next_idx < self.pieces.len() && offset >= piece_end {
                let next_piece = &self.pieces[next_idx];
                let next_end = piece_end + next_piece.length;
                if offset < next_end {
                    self.access_cache.set(Some(PieceCache {
                        piece_index: next_idx,
                        logical_start: piece_end,
                    }));
                    return Some((next_idx, offset - piece_end, piece_end));
                }
            }
        }

        // Linear scan
        let mut logical_pos = 0;
        for (i, piece) in self.pieces.iter().enumerate() {
            let piece_end = logical_pos + piece.length;
            if offset < piece_end {
                self.access_cache.set(Some(PieceCache {
                    piece_index: i,
                    logical_start: logical_pos,
                }));
                return Some((i, offset - logical_pos, logical_pos));
            }
            logical_pos = piece_end;
        }

        None
    }

    /// Find the piece index at which to insert at the given logical offset.
    /// Returns (piece_index, offset_within_piece, logical_start_of_piece).
    /// For offset == cached_length, returns (pieces.len(), 0, cached_length).
    fn find_insert_point(&self, offset: usize) -> Option<(usize, usize, usize)> {
        if offset > self.cached_length {
            return None;
        }
        if offset == self.cached_length {
            return Some((self.pieces.len(), 0, self.cached_length));
        }
        self.find_piece(offset)
    }

    /// Split piece at offset, inserting new pieces in the middle.
    /// This is the core operation for insert/replace/delete.
    fn split_and_insert(
        &mut self,
        piece_idx: usize,
        offset_in_piece: usize,
        new_pieces: Vec<Piece>,
    ) {
        if piece_idx >= self.pieces.len() {
            // Append at end
            self.pieces.extend(new_pieces);
            return;
        }

        let original = self.pieces[piece_idx].clone();

        let mut replacement = Vec::new();

        // Left part (before split point)
        if offset_in_piece > 0 {
            replacement.push(Piece {
                source: original.source,
                start: original.start,
                length: offset_in_piece,
            });
        }

        // New pieces
        replacement.extend(new_pieces);

        // Right part (after split point)
        let right_len = original.length - offset_in_piece;
        if right_len > 0 {
            replacement.push(Piece {
                source: original.source,
                start: original.start + offset_in_piece,
                length: right_len,
            });
        }

        self.pieces.splice(piece_idx..=piece_idx, replacement);
        self.access_cache.set(None);
    }

    // =====================================================
    // Public API
    // =====================================================

    /// Get byte at offset
    pub fn get_byte(&self, offset: usize) -> Option<u8> {
        let (piece_idx, offset_in_piece, _) = self.find_piece(offset)?;
        self.get_byte_from_source(&self.pieces[piece_idx], offset_in_piece)
    }

    /// Get slice of data by walking pieces directly
    #[allow(dead_code)]
    pub fn get_slice(&self, range: std::ops::Range<usize>) -> Option<Vec<u8>> {
        if range.is_empty() {
            return Some(Vec::new());
        }
        if range.end > self.cached_length {
            return None;
        }

        let mut result = Vec::with_capacity(range.len());
        let mut remaining_start = range.start;
        let remaining_end = range.end;

        let mut logical_pos = 0;
        for piece in &self.pieces {
            let piece_end = logical_pos + piece.length;

            // Skip pieces entirely before range
            if piece_end <= remaining_start {
                logical_pos = piece_end;
                continue;
            }
            // Stop if we've passed the range
            if logical_pos >= remaining_end {
                break;
            }

            let slice = self.piece_slice(piece);
            let copy_start = remaining_start.saturating_sub(logical_pos);
            let copy_end = (remaining_end - logical_pos).min(piece.length);
            result.extend_from_slice(&slice[copy_start..copy_end]);

            remaining_start = piece_end;
            logical_pos = piece_end;
        }

        Some(result)
    }

    /// Get document length
    pub fn len(&self) -> usize {
        self.cached_length
    }

    /// Check if document is empty
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.cached_length == 0
    }

    /// Set byte at offset (backward compatible with old API)
    /// Push an undo record, trimming oldest if over limit, and clear redo stack.
    fn push_undo_record(&mut self, operation: EditOperation) {
        let undo_record = UndoRecord {
            operation,
            pieces_snapshot: self.pieces.clone(),
            cached_length: self.cached_length,
        };
        self.undo_stack.push(undo_record);
        if self.undo_stack.len() > self.max_undo_levels {
            self.undo_stack.remove(0);
        }
        self.redo_stack.clear();
    }

    pub fn set_byte(&mut self, offset: usize, value: u8) -> Result<(), String> {
        if offset >= self.cached_length {
            return Err(format!(
                "Offset {} out of bounds (size: {})",
                offset, self.cached_length
            ));
        }

        let old_value = self.get_byte(offset).unwrap();

        if old_value != value {
            self.push_undo_record(EditOperation::Replace {
                offset,
                old_bytes: vec![old_value],
                new_bytes: vec![value],
            });

            // Add new byte to add_buffer
            let add_offset = self.add_buffer.len();
            self.add_buffer.push(value);

            // Find and split the piece
            let (piece_idx, offset_in_piece, _) = self.find_piece(offset).unwrap();
            let original = self.pieces[piece_idx].clone();

            let mut replacement = Vec::new();

            // Left part
            if offset_in_piece > 0 {
                replacement.push(Piece {
                    source: original.source,
                    start: original.start,
                    length: offset_in_piece,
                });
            }

            // New 1-byte piece
            replacement.push(Piece {
                source: PieceSource::Add,
                start: add_offset,
                length: 1,
            });

            // Right part
            let right_len = original.length - offset_in_piece - 1;
            if right_len > 0 {
                replacement.push(Piece {
                    source: original.source,
                    start: original.start + offset_in_piece + 1,
                    length: right_len,
                });
            }

            self.pieces.splice(piece_idx..=piece_idx, replacement);
            self.access_cache.set(None);
            self.has_unsaved_changes = true;
        }

        Ok(())
    }

    /// Insert bytes at offset
    pub fn insert_bytes(&mut self, offset: usize, data: &[u8]) -> Result<(), String> {
        if offset > self.cached_length {
            return Err(format!(
                "Offset {} out of bounds (size: {})",
                offset, self.cached_length
            ));
        }

        if data.is_empty() {
            return Ok(());
        }

        self.push_undo_record(EditOperation::Insert {
            offset,
            data: data.to_vec(),
        });

        // Add data to add_buffer
        let add_offset = self.add_buffer.len();
        self.add_buffer.extend_from_slice(data);

        let new_piece = Piece {
            source: PieceSource::Add,
            start: add_offset,
            length: data.len(),
        };

        if let Some((piece_idx, offset_in_piece, _)) = self.find_insert_point(offset) {
            if piece_idx >= self.pieces.len() {
                // Append at end
                self.pieces.push(new_piece);
            } else if offset_in_piece == 0 {
                // Insert before piece
                self.pieces.insert(piece_idx, new_piece);
            } else {
                self.split_and_insert(piece_idx, offset_in_piece, vec![new_piece]);
            }
        }

        self.cached_length += data.len();
        self.access_cache.set(None);
        self.has_unsaved_changes = true;

        Ok(())
    }

    /// Delete bytes starting at offset
    pub fn delete_bytes(&mut self, offset: usize, count: usize) -> Result<(), String> {
        if count == 0 {
            return Ok(());
        }

        let end = offset + count;
        if end > self.cached_length {
            return Err(format!(
                "Delete range {}..{} out of bounds (size: {})",
                offset, end, self.cached_length
            ));
        }

        // Collect deleted bytes for undo (bulk copy via piece table)
        let deleted_bytes = self.get_slice(offset..end).unwrap_or_default();

        self.push_undo_record(EditOperation::Delete {
            offset,
            deleted_bytes,
        });

        // Find the range of pieces affected
        self.delete_piece_range(offset, count);

        self.cached_length -= count;
        self.access_cache.set(None);
        self.has_unsaved_changes = true;

        Ok(())
    }

    /// Internal: delete a range from the piece table
    fn delete_piece_range(&mut self, offset: usize, count: usize) {
        let end = offset + count;

        // Find first piece affected
        let mut logical_pos = 0;
        let mut first_idx = None;
        let mut first_offset_in_piece = 0;

        for (i, piece) in self.pieces.iter().enumerate() {
            let piece_end = logical_pos + piece.length;
            if offset < piece_end {
                first_idx = Some(i);
                first_offset_in_piece = offset - logical_pos;
                break;
            }
            logical_pos = piece_end;
        }

        let first_idx = match first_idx {
            Some(idx) => idx,
            None => return,
        };

        // Find last piece affected
        logical_pos = 0;
        let mut last_idx = first_idx;
        let mut last_offset_in_piece_end = 0;

        for (i, piece) in self.pieces.iter().enumerate() {
            let piece_end = logical_pos + piece.length;
            if end <= piece_end {
                last_idx = i;
                last_offset_in_piece_end = end - logical_pos;
                break;
            }
            logical_pos = piece_end;
        }

        // Build replacement pieces
        let mut replacement = Vec::new();

        // Left part of first piece (before deletion start)
        if first_offset_in_piece > 0 {
            let first_piece = &self.pieces[first_idx];
            replacement.push(Piece {
                source: first_piece.source,
                start: first_piece.start,
                length: first_offset_in_piece,
            });
        }

        // Right part of last piece (after deletion end)
        let last_piece = &self.pieces[last_idx];
        let right_len = last_piece.length - last_offset_in_piece_end;
        if right_len > 0 {
            replacement.push(Piece {
                source: last_piece.source,
                start: last_piece.start + last_offset_in_piece_end,
                length: right_len,
            });
        }

        self.pieces.splice(first_idx..=last_idx, replacement);
    }

    /// Replace bytes starting at offset
    #[allow(dead_code)]
    pub fn replace_bytes(
        &mut self,
        offset: usize,
        count: usize,
        data: &[u8],
    ) -> Result<(), String> {
        let end = offset + count;
        if end > self.cached_length {
            return Err(format!(
                "Replace range {}..{} out of bounds (size: {})",
                offset, end, self.cached_length
            ));
        }

        if count == 0 && data.is_empty() {
            return Ok(());
        }

        // Collect old bytes for undo (bulk copy via piece table)
        let old_bytes = self.get_slice(offset..end).unwrap_or_default();

        self.push_undo_record(EditOperation::Replace {
            offset,
            old_bytes,
            new_bytes: data.to_vec(),
        });

        // Delete old range
        if count > 0 {
            self.delete_piece_range(offset, count);
            self.cached_length -= count;
        }

        // Insert new data
        if !data.is_empty() {
            let add_offset = self.add_buffer.len();
            self.add_buffer.extend_from_slice(data);

            let new_piece = Piece {
                source: PieceSource::Add,
                start: add_offset,
                length: data.len(),
            };

            if let Some((piece_idx, offset_in_piece, _)) = self.find_insert_point(offset) {
                if piece_idx >= self.pieces.len() {
                    self.pieces.push(new_piece);
                } else if offset_in_piece == 0 {
                    self.pieces.insert(piece_idx, new_piece);
                } else {
                    self.split_and_insert(piece_idx, offset_in_piece, vec![new_piece]);
                }
            }

            self.cached_length += data.len();
        }

        self.access_cache.set(None);
        self.has_unsaved_changes = true;

        Ok(())
    }

    /// Undo last edit. Returns the offset of the undone edit.
    pub fn undo(&mut self) -> Option<usize> {
        if let Some(record) = self.undo_stack.pop() {
            let offset = match &record.operation {
                EditOperation::Replace { offset, .. } => *offset,
                EditOperation::Insert { offset, .. } => *offset,
                EditOperation::Delete { offset, .. } => *offset,
            };

            // Save current state for redo
            let redo_record = UndoRecord {
                operation: record.operation.clone(),
                pieces_snapshot: self.pieces.clone(),
                cached_length: self.cached_length,
            };
            self.redo_stack.push(redo_record);

            // Restore piece table from snapshot
            self.pieces = record.pieces_snapshot;
            self.cached_length = record.cached_length;
            self.access_cache.set(None);

            self.has_unsaved_changes = !self.undo_stack.is_empty();

            Some(offset)
        } else {
            None
        }
    }

    /// Redo previously undone edit. Returns the offset of the redone edit.
    pub fn redo(&mut self) -> Option<usize> {
        if let Some(record) = self.redo_stack.pop() {
            let offset = match &record.operation {
                EditOperation::Replace { offset, .. } => *offset,
                EditOperation::Insert { offset, .. } => *offset,
                EditOperation::Delete { offset, .. } => *offset,
            };

            // Save current state for undo
            let undo_record = UndoRecord {
                operation: record.operation.clone(),
                pieces_snapshot: self.pieces.clone(),
                cached_length: self.cached_length,
            };
            self.undo_stack.push(undo_record);

            // Restore piece table from redo snapshot
            self.pieces = record.pieces_snapshot;
            self.cached_length = record.cached_length;
            self.access_cache.set(None);

            self.has_unsaved_changes = true;

            Some(offset)
        } else {
            None
        }
    }

    /// Check if undo is available
    pub fn can_undo(&self) -> bool {
        !self.undo_stack.is_empty()
    }

    /// Check if redo is available
    pub fn can_redo(&self) -> bool {
        !self.redo_stack.is_empty()
    }

    /// Check if document has unsaved changes
    pub fn has_unsaved_changes(&self) -> bool {
        self.has_unsaved_changes
    }

    /// Get file path
    pub fn file_path(&self) -> Option<&PathBuf> {
        self.file_path.as_ref()
    }

    /// Get file name (display_name takes priority if set)
    pub fn file_name(&self) -> Option<&str> {
        if let Some(name) = &self.display_name {
            return Some(name.as_str());
        }
        self.file_path
            .as_ref()
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
    }

    /// Set display name override
    pub fn set_display_name(&mut self, name: String) {
        self.display_name = Some(name);
    }

    /// Get number of modified bytes (approximation: number of add_buffer pieces)
    #[allow(dead_code)]
    pub fn modified_count(&self) -> usize {
        self.pieces
            .iter()
            .filter(|p| p.source == PieceSource::Add)
            .count()
    }

    /// Check if byte at offset is modified (comes from add buffer)
    #[allow(dead_code)]
    pub fn is_modified(&self, offset: usize) -> bool {
        if let Some((piece_idx, _, _)) = self.find_piece(offset) {
            self.pieces[piece_idx].source == PieceSource::Add
        } else {
            false
        }
    }

    /// Materialize all document data into a Vec
    pub fn to_vec(&self) -> Vec<u8> {
        let mut result = Vec::with_capacity(self.cached_length);
        for piece in &self.pieces {
            match piece.source {
                PieceSource::Original => match &self.original {
                    OriginalBuffer::InMemory(data) => {
                        let end = (piece.start + piece.length).min(data.len());
                        if piece.start < end {
                            result.extend_from_slice(&data[piece.start..end]);
                        }
                    }
                    OriginalBuffer::MemoryMapped { mmap, .. } => {
                        let end = (piece.start + piece.length).min(mmap.len());
                        if piece.start < end {
                            result.extend_from_slice(&mmap[piece.start..end]);
                        }
                    }
                },
                PieceSource::Add => {
                    let end = (piece.start + piece.length).min(self.add_buffer.len());
                    if piece.start < end {
                        result.extend_from_slice(&self.add_buffer[piece.start..end]);
                    }
                }
            }
        }
        result
    }

    /// Prepare data source for background search
    pub fn prepare_search_data(&self) -> SearchDataSource {
        SearchDataSource::InMemory(self.to_vec())
    }

    /// Merge adjacent pieces from the same source with contiguous offsets
    #[allow(dead_code)]
    pub fn coalesce_pieces(&mut self) {
        if self.pieces.len() <= 1 {
            return;
        }

        let mut coalesced = Vec::with_capacity(self.pieces.len());
        let mut current = self.pieces[0].clone();

        for piece in self.pieces.iter().skip(1) {
            if piece.source == current.source && piece.start == current.start + current.length {
                // Merge
                current.length += piece.length;
            } else {
                coalesced.push(current);
                current = piece.clone();
            }
        }
        coalesced.push(current);

        self.pieces = coalesced;
        self.access_cache.set(None);
    }
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // =========================================
    // Basic operations
    // =========================================

    #[test]
    fn test_new_empty_document() {
        let doc = Document::new();
        assert_eq!(doc.len(), 0);
        assert!(doc.is_empty());
        assert_eq!(doc.get_byte(0), None);
    }

    #[test]
    fn test_with_data() {
        let doc = Document::with_data(vec![0x41, 0x42, 0x43]);
        assert_eq!(doc.len(), 3);
        assert!(!doc.is_empty());
        assert_eq!(doc.get_byte(0), Some(0x41));
        assert_eq!(doc.get_byte(1), Some(0x42));
        assert_eq!(doc.get_byte(2), Some(0x43));
        assert_eq!(doc.get_byte(3), None);
    }

    #[test]
    fn test_get_byte_basic() {
        let data: Vec<u8> = (0..=255).collect();
        let doc = Document::with_data(data);
        assert_eq!(doc.len(), 256);
        for i in 0..256 {
            assert_eq!(doc.get_byte(i), Some(i as u8));
        }
    }

    #[test]
    fn test_len() {
        let doc = Document::with_data(vec![1, 2, 3, 4, 5]);
        assert_eq!(doc.len(), 5);
    }

    // =========================================
    // set_byte (backward compatible)
    // =========================================

    #[test]
    fn test_set_byte_single() {
        let mut doc = Document::with_data(vec![0x00, 0x01, 0x02]);
        doc.set_byte(1, 0xFF).unwrap();
        assert_eq!(doc.get_byte(0), Some(0x00));
        assert_eq!(doc.get_byte(1), Some(0xFF));
        assert_eq!(doc.get_byte(2), Some(0x02));
    }

    #[test]
    fn test_set_byte_preserves_length() {
        let mut doc = Document::with_data(vec![0x00; 10]);
        doc.set_byte(5, 0xAA).unwrap();
        assert_eq!(doc.len(), 10);
    }

    #[test]
    fn test_set_byte_multiple() {
        let mut doc = Document::with_data(vec![0x00; 5]);
        doc.set_byte(0, 0x01).unwrap();
        doc.set_byte(2, 0x02).unwrap();
        doc.set_byte(4, 0x03).unwrap();
        assert_eq!(doc.get_byte(0), Some(0x01));
        assert_eq!(doc.get_byte(1), Some(0x00));
        assert_eq!(doc.get_byte(2), Some(0x02));
        assert_eq!(doc.get_byte(3), Some(0x00));
        assert_eq!(doc.get_byte(4), Some(0x03));
    }

    #[test]
    fn test_set_byte_out_of_bounds() {
        let mut doc = Document::with_data(vec![0x00; 3]);
        assert!(doc.set_byte(3, 0xFF).is_err());
    }

    #[test]
    fn test_set_byte_same_value_no_change() {
        let mut doc = Document::with_data(vec![0x42]);
        doc.set_byte(0, 0x42).unwrap();
        assert!(!doc.has_unsaved_changes());
        assert!(!doc.can_undo());
    }

    // =========================================
    // insert_bytes
    // =========================================

    #[test]
    fn test_insert_at_beginning() {
        let mut doc = Document::with_data(vec![0x03, 0x04, 0x05]);
        doc.insert_bytes(0, &[0x01, 0x02]).unwrap();
        assert_eq!(doc.len(), 5);
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03, 0x04, 0x05]);
    }

    #[test]
    fn test_insert_at_end() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.insert_bytes(3, &[0x04, 0x05]).unwrap();
        assert_eq!(doc.len(), 5);
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03, 0x04, 0x05]);
    }

    #[test]
    fn test_insert_in_middle() {
        let mut doc = Document::with_data(vec![0x01, 0x04, 0x05]);
        doc.insert_bytes(1, &[0x02, 0x03]).unwrap();
        assert_eq!(doc.len(), 5);
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03, 0x04, 0x05]);
    }

    #[test]
    fn test_insert_increases_length() {
        let mut doc = Document::with_data(vec![0x01]);
        assert_eq!(doc.len(), 1);
        doc.insert_bytes(1, &[0x02, 0x03, 0x04]).unwrap();
        assert_eq!(doc.len(), 4);
    }

    #[test]
    fn test_insert_into_empty() {
        let mut doc = Document::new();
        doc.insert_bytes(0, &[0x41, 0x42, 0x43]).unwrap();
        assert_eq!(doc.len(), 3);
        assert_eq!(doc.to_vec(), vec![0x41, 0x42, 0x43]);
    }

    #[test]
    fn test_insert_out_of_bounds() {
        let mut doc = Document::with_data(vec![0x01]);
        assert!(doc.insert_bytes(5, &[0x02]).is_err());
    }

    #[test]
    fn test_insert_empty_data() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        doc.insert_bytes(1, &[]).unwrap();
        assert_eq!(doc.len(), 2);
        assert_eq!(doc.to_vec(), vec![0x01, 0x02]);
    }

    // =========================================
    // delete_bytes
    // =========================================

    #[test]
    fn test_delete_at_beginning() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.delete_bytes(0, 2).unwrap();
        assert_eq!(doc.len(), 3);
        assert_eq!(doc.to_vec(), vec![0x03, 0x04, 0x05]);
    }

    #[test]
    fn test_delete_at_end() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.delete_bytes(3, 2).unwrap();
        assert_eq!(doc.len(), 3);
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03]);
    }

    #[test]
    fn test_delete_in_middle() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.delete_bytes(1, 3).unwrap();
        assert_eq!(doc.len(), 2);
        assert_eq!(doc.to_vec(), vec![0x01, 0x05]);
    }

    #[test]
    fn test_delete_spanning_pieces() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        // Create multiple pieces by inserting
        doc.insert_bytes(1, &[0xAA, 0xBB]).unwrap();
        // Now: [0x01, 0xAA, 0xBB, 0x02, 0x03]
        doc.delete_bytes(0, 3).unwrap();
        // Should be: [0x02, 0x03]
        assert_eq!(doc.len(), 2);
        assert_eq!(doc.to_vec(), vec![0x02, 0x03]);
    }

    #[test]
    fn test_delete_entire_document() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.delete_bytes(0, 3).unwrap();
        assert_eq!(doc.len(), 0);
        assert!(doc.is_empty());
        assert_eq!(doc.to_vec(), Vec::<u8>::new());
    }

    #[test]
    fn test_delete_out_of_bounds() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        assert!(doc.delete_bytes(1, 5).is_err());
    }

    #[test]
    fn test_delete_zero_count() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.delete_bytes(1, 0).unwrap();
        assert_eq!(doc.len(), 3);
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03]);
    }

    // =========================================
    // undo/redo
    // =========================================

    #[test]
    fn test_undo_set_byte() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(1, 0xFF).unwrap();
        assert_eq!(doc.get_byte(1), Some(0xFF));

        doc.undo();
        assert_eq!(doc.get_byte(1), Some(0x02));
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03]);
    }

    #[test]
    fn test_undo_insert() {
        let mut doc = Document::with_data(vec![0x01, 0x03]);
        doc.insert_bytes(1, &[0x02]).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03]);

        doc.undo();
        assert_eq!(doc.to_vec(), vec![0x01, 0x03]);
        assert_eq!(doc.len(), 2);
    }

    #[test]
    fn test_undo_delete() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.delete_bytes(1, 1).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x03]);

        doc.undo();
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03]);
        assert_eq!(doc.len(), 3);
    }

    #[test]
    fn test_redo_after_undo() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(0, 0xFF).unwrap();
        assert_eq!(doc.get_byte(0), Some(0xFF));

        doc.undo();
        assert_eq!(doc.get_byte(0), Some(0x01));

        doc.redo();
        assert_eq!(doc.get_byte(0), Some(0xFF));
    }

    #[test]
    fn test_new_edit_clears_redo() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(0, 0xFF).unwrap();
        doc.undo();
        assert!(doc.can_redo());

        doc.set_byte(0, 0xAA).unwrap();
        assert!(!doc.can_redo());
    }

    #[test]
    fn test_undo_redo_preserves_data() {
        let original = vec![0x01, 0x02, 0x03, 0x04, 0x05];
        let mut doc = Document::with_data(original.clone());

        // Multiple edits
        doc.set_byte(0, 0xAA).unwrap();
        doc.insert_bytes(2, &[0xBB, 0xCC]).unwrap();
        doc.delete_bytes(5, 2).unwrap();

        // Undo all
        doc.undo();
        doc.undo();
        doc.undo();
        assert_eq!(doc.to_vec(), original);

        // Redo all
        doc.redo();
        doc.redo();
        doc.redo();

        // Undo all again
        doc.undo();
        doc.undo();
        doc.undo();
        assert_eq!(doc.to_vec(), original);
    }

    // =========================================
    // to_vec / data consistency
    // =========================================

    #[test]
    fn test_to_vec_after_edits() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(0, 0xAA).unwrap();
        doc.set_byte(2, 0xCC).unwrap();
        assert_eq!(doc.to_vec(), vec![0xAA, 0x02, 0xCC]);
    }

    #[test]
    fn test_to_vec_after_insert_delete() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.insert_bytes(1, &[0xAA]).unwrap();
        // [0x01, 0xAA, 0x02, 0x03]
        doc.delete_bytes(2, 1).unwrap();
        // [0x01, 0xAA, 0x03]
        assert_eq!(doc.to_vec(), vec![0x01, 0xAA, 0x03]);
    }

    // =========================================
    // coalesce
    // =========================================

    #[test]
    fn test_coalesce_merges_adjacent() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        // Multiple single-byte edits at consecutive positions should create
        // many pieces, but coalesce should not merge them since they reference
        // different add_buffer offsets (not contiguous in source)
        let initial_piece_count = doc.pieces.len();
        assert_eq!(initial_piece_count, 1);

        // However, if we manually create adjacent pieces...
        doc.pieces = vec![
            Piece {
                source: PieceSource::Original,
                start: 0,
                length: 2,
            },
            Piece {
                source: PieceSource::Original,
                start: 2,
                length: 3,
            },
        ];
        doc.coalesce_pieces();
        assert_eq!(doc.pieces.len(), 1);
        assert_eq!(doc.pieces[0].length, 5);
    }

    #[test]
    fn test_coalesce_does_not_merge_different_sources() {
        let mut doc = Document::new();
        doc.add_buffer = vec![0xAA, 0xBB];
        doc.pieces = vec![
            Piece {
                source: PieceSource::Original,
                start: 0,
                length: 2,
            },
            Piece {
                source: PieceSource::Add,
                start: 0,
                length: 2,
            },
        ];
        doc.coalesce_pieces();
        assert_eq!(doc.pieces.len(), 2);
    }

    // =========================================
    // Sequential access cache
    // =========================================

    #[test]
    fn test_sequential_access_performance() {
        let data: Vec<u8> = (0..1000).map(|i| (i % 256) as u8).collect();
        let doc = Document::with_data(data.clone());

        // Sequential access should be fast due to caching
        for (i, expected) in data.iter().enumerate() {
            assert_eq!(doc.get_byte(i), Some(*expected));
        }
    }

    // =========================================
    // replace_bytes
    // =========================================

    #[test]
    fn test_replace_bytes_same_length() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.replace_bytes(1, 2, &[0xAA, 0xBB]).unwrap();
        assert_eq!(doc.len(), 5);
        assert_eq!(doc.to_vec(), vec![0x01, 0xAA, 0xBB, 0x04, 0x05]);
    }

    #[test]
    fn test_replace_bytes_shorter() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.replace_bytes(1, 3, &[0xAA]).unwrap();
        assert_eq!(doc.len(), 3);
        assert_eq!(doc.to_vec(), vec![0x01, 0xAA, 0x05]);
    }

    #[test]
    fn test_replace_bytes_longer() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.replace_bytes(1, 1, &[0xAA, 0xBB, 0xCC]).unwrap();
        assert_eq!(doc.len(), 5);
        assert_eq!(doc.to_vec(), vec![0x01, 0xAA, 0xBB, 0xCC, 0x03]);
    }

    // =========================================
    // Edge cases
    // =========================================

    #[test]
    fn test_multiple_inserts() {
        let mut doc = Document::new();
        doc.insert_bytes(0, &[0x03]).unwrap();
        doc.insert_bytes(0, &[0x01]).unwrap();
        doc.insert_bytes(1, &[0x02]).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03]);
    }

    #[test]
    fn test_interleaved_operations() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.set_byte(2, 0xFF).unwrap();
        doc.insert_bytes(3, &[0xAA]).unwrap();
        doc.delete_bytes(0, 1).unwrap();
        // Started: [01, 02, 03, 04, 05]
        // After set: [01, 02, FF, 04, 05]
        // After insert: [01, 02, FF, AA, 04, 05]
        // After delete: [02, FF, AA, 04, 05]
        assert_eq!(doc.to_vec(), vec![0x02, 0xFF, 0xAA, 0x04, 0x05]);
    }

    #[test]
    fn test_get_byte_after_many_edits() {
        let mut doc = Document::with_data(vec![0; 100]);
        // Set every other byte
        for i in (0..100).step_by(2) {
            doc.set_byte(i, 0xFF).unwrap();
        }
        for i in 0..100 {
            if i % 2 == 0 {
                assert_eq!(doc.get_byte(i), Some(0xFF));
            } else {
                assert_eq!(doc.get_byte(i), Some(0x00));
            }
        }
    }

    #[test]
    fn test_prepare_search_data() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(1, 0xFF).unwrap();
        match doc.prepare_search_data() {
            SearchDataSource::InMemory(data) => {
                assert_eq!(data, vec![0x01, 0xFF, 0x03]);
            }
        }
    }

    #[test]
    fn test_has_unsaved_changes_after_undo_all() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        assert!(!doc.has_unsaved_changes());

        doc.set_byte(0, 0xFF).unwrap();
        assert!(doc.has_unsaved_changes());

        doc.undo();
        assert!(!doc.has_unsaved_changes());
    }

    // =========================================
    // get_slice (bulk read)
    // =========================================

    #[test]
    fn test_get_slice_single_piece() {
        let doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        assert_eq!(doc.get_slice(1..4), Some(vec![0x02, 0x03, 0x04]));
    }

    #[test]
    fn test_get_slice_spanning_pieces() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.set_byte(2, 0xFF).unwrap();
        // pieces: [orig 0..2], [add FF], [orig 3..5]
        assert_eq!(
            doc.get_slice(0..5),
            Some(vec![0x01, 0x02, 0xFF, 0x04, 0x05])
        );
        // Partial span across piece boundaries
        assert_eq!(doc.get_slice(1..4), Some(vec![0x02, 0xFF, 0x04]));
    }

    #[test]
    fn test_get_slice_empty_range() {
        let doc = Document::with_data(vec![0x01, 0x02]);
        assert_eq!(doc.get_slice(1..1), Some(vec![]));
    }

    #[test]
    fn test_get_slice_out_of_bounds() {
        let doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        assert_eq!(doc.get_slice(0..5), None);
    }

    #[test]
    fn test_get_slice_after_insert() {
        let mut doc = Document::with_data(vec![0x01, 0x04, 0x05]);
        doc.insert_bytes(1, &[0x02, 0x03]).unwrap();
        // [0x01, 0x02, 0x03, 0x04, 0x05]
        assert_eq!(
            doc.get_slice(0..5),
            Some(vec![0x01, 0x02, 0x03, 0x04, 0x05])
        );
    }

    #[test]
    fn test_get_slice_matches_to_vec() {
        let mut doc = Document::with_data(vec![0; 100]);
        for i in (0..100).step_by(3) {
            doc.set_byte(i, (i as u8).wrapping_mul(7)).unwrap();
        }
        doc.insert_bytes(50, &[0xAA, 0xBB, 0xCC]).unwrap();
        let full = doc.to_vec();
        // get_slice of the entire document should match to_vec
        assert_eq!(doc.get_slice(0..doc.len()), Some(full.clone()));
        // Sub-ranges should match too
        assert_eq!(doc.get_slice(10..60), Some(full[10..60].to_vec()));
    }

    // =========================================
    // write_to (streaming write)
    // =========================================

    #[test]
    fn test_write_to_matches_to_vec() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.set_byte(2, 0xFF).unwrap();
        doc.insert_bytes(3, &[0xAA, 0xBB]).unwrap();

        let mut buf = Vec::new();
        doc.write_to(&mut buf).unwrap();
        assert_eq!(buf, doc.to_vec());
    }

    #[test]
    fn test_write_to_empty_doc() {
        let doc = Document::new();
        let mut buf = Vec::new();
        doc.write_to(&mut buf).unwrap();
        assert!(buf.is_empty());
    }

    // =========================================
    // write_range_to (streaming range write)
    // =========================================

    #[test]
    fn test_write_range_to_matches_get_slice() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.set_byte(2, 0xFF).unwrap();
        doc.insert_bytes(3, &[0xAA, 0xBB]).unwrap();

        let mut buf = Vec::new();
        doc.write_range_to(1..5, &mut buf).unwrap();
        assert_eq!(buf, doc.get_slice(1..5).unwrap());
    }

    #[test]
    fn test_write_range_to_full_range() {
        let mut doc = Document::with_data(vec![0; 50]);
        for i in (0..50).step_by(5) {
            doc.set_byte(i, 0xAA).unwrap();
        }
        let mut buf = Vec::new();
        doc.write_range_to(0..doc.len(), &mut buf).unwrap();
        assert_eq!(buf, doc.to_vec());
    }

    #[test]
    fn test_write_range_to_empty_range() {
        let doc = Document::with_data(vec![0x01, 0x02]);
        let mut buf = Vec::new();
        doc.write_range_to(1..1, &mut buf).unwrap();
        assert!(buf.is_empty());
    }

    #[test]
    fn test_write_range_to_out_of_bounds() {
        let doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        let mut buf = Vec::new();
        assert!(doc.write_range_to(0..10, &mut buf).is_err());
    }

    // =========================================
    // display_name
    // =========================================

    #[test]
    fn test_file_name_returns_none_for_new_document() {
        let doc = Document::new();
        assert_eq!(doc.file_name(), None);
    }

    #[test]
    fn test_display_name_overrides_file_name() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        doc.set_display_name("test [0x0-0x1]".to_string());
        assert_eq!(doc.file_name(), Some("test [0x0-0x1]"));
    }

    #[test]
    fn test_display_name_with_data() {
        let mut doc = Document::with_data(vec![0xDE, 0xAD, 0xBE, 0xEF]);
        assert_eq!(doc.file_name(), None);
        doc.set_display_name("selection.bin".to_string());
        assert_eq!(doc.file_name(), Some("selection.bin"));
        assert_eq!(doc.len(), 4);
        assert_eq!(doc.get_byte(0), Some(0xDE));
    }

    // =========================================
    // Edge cases: empty document operations
    // =========================================

    #[test]
    fn test_set_byte_on_empty_doc_fails() {
        let mut doc = Document::new();
        assert!(doc.set_byte(0, 0xFF).is_err());
    }

    #[test]
    fn test_delete_on_empty_doc_fails() {
        let mut doc = Document::new();
        assert!(doc.delete_bytes(0, 1).is_err());
    }

    #[test]
    fn test_get_slice_on_empty_doc() {
        let doc = Document::new();
        assert_eq!(doc.get_slice(0..0), Some(vec![]));
        assert_eq!(doc.get_slice(0..1), None);
    }

    #[test]
    fn test_to_vec_empty_doc() {
        let doc = Document::new();
        assert_eq!(doc.to_vec(), Vec::<u8>::new());
    }

    #[test]
    fn test_undo_on_empty_doc_returns_none() {
        let mut doc = Document::new();
        assert!(doc.undo().is_none());
    }

    #[test]
    fn test_redo_on_empty_doc_returns_none() {
        let mut doc = Document::new();
        assert!(doc.redo().is_none());
    }

    // =========================================
    // Edge cases: single-byte document
    // =========================================

    #[test]
    fn test_single_byte_set() {
        let mut doc = Document::with_data(vec![0x00]);
        doc.set_byte(0, 0xFF).unwrap();
        assert_eq!(doc.get_byte(0), Some(0xFF));
        assert_eq!(doc.len(), 1);
    }

    #[test]
    fn test_single_byte_delete() {
        let mut doc = Document::with_data(vec![0x42]);
        doc.delete_bytes(0, 1).unwrap();
        assert_eq!(doc.len(), 0);
        assert!(doc.is_empty());
        assert_eq!(doc.get_byte(0), None);
    }

    #[test]
    fn test_single_byte_insert_before() {
        let mut doc = Document::with_data(vec![0x02]);
        doc.insert_bytes(0, &[0x01]).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x02]);
    }

    #[test]
    fn test_single_byte_insert_after() {
        let mut doc = Document::with_data(vec![0x01]);
        doc.insert_bytes(1, &[0x02]).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x02]);
    }

    // =========================================
    // Edge cases: boundary byte values
    // =========================================

    #[test]
    fn test_all_byte_values_roundtrip() {
        let data: Vec<u8> = (0..=255).collect();
        let doc = Document::with_data(data.clone());
        assert_eq!(doc.to_vec(), data);
    }

    #[test]
    fn test_set_byte_to_zero() {
        let mut doc = Document::with_data(vec![0xFF]);
        doc.set_byte(0, 0x00).unwrap();
        assert_eq!(doc.get_byte(0), Some(0x00));
    }

    #[test]
    fn test_set_byte_to_max() {
        let mut doc = Document::with_data(vec![0x00]);
        doc.set_byte(0, 0xFF).unwrap();
        assert_eq!(doc.get_byte(0), Some(0xFF));
    }

    // =========================================
    // Edge cases: consecutive undo/redo
    // =========================================

    #[test]
    fn test_multiple_undo_past_beginning() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        doc.set_byte(0, 0xAA).unwrap();

        assert!(doc.undo().is_some());
        assert!(doc.undo().is_none()); // no more undos
        assert!(doc.undo().is_none()); // still none
        assert_eq!(doc.to_vec(), vec![0x01, 0x02]);
    }

    #[test]
    fn test_multiple_redo_past_end() {
        let mut doc = Document::with_data(vec![0x01]);
        doc.set_byte(0, 0xAA).unwrap();
        doc.undo();

        assert!(doc.redo().is_some());
        assert!(doc.redo().is_none()); // no more redos
        assert!(doc.redo().is_none());
        assert_eq!(doc.get_byte(0), Some(0xAA));
    }

    #[test]
    fn test_undo_redo_undo_redo_cycle() {
        let original = vec![0x01, 0x02, 0x03];
        let mut doc = Document::with_data(original.clone());
        doc.set_byte(0, 0xFF).unwrap();

        for _ in 0..5 {
            doc.undo();
            assert_eq!(doc.to_vec(), original);
            doc.redo();
            assert_eq!(doc.get_byte(0), Some(0xFF));
        }
    }

    // =========================================
    // Edge cases: undo/redo with insert and delete
    // =========================================

    #[test]
    fn test_undo_insert_into_empty_restores_empty() {
        let mut doc = Document::new();
        doc.insert_bytes(0, &[0x01, 0x02, 0x03]).unwrap();
        assert_eq!(doc.len(), 3);

        doc.undo();
        assert_eq!(doc.len(), 0);
        assert!(doc.is_empty());
        assert_eq!(doc.to_vec(), Vec::<u8>::new());
    }

    #[test]
    fn test_undo_delete_all_restores_content() {
        let original = vec![0x01, 0x02, 0x03, 0x04, 0x05];
        let mut doc = Document::with_data(original.clone());
        doc.delete_bytes(0, 5).unwrap();
        assert!(doc.is_empty());

        doc.undo();
        assert_eq!(doc.to_vec(), original);
    }

    #[test]
    fn test_redo_insert_after_undo() {
        let mut doc = Document::new();
        doc.insert_bytes(0, &[0xAA, 0xBB]).unwrap();
        doc.undo();
        assert!(doc.is_empty());

        doc.redo();
        assert_eq!(doc.to_vec(), vec![0xAA, 0xBB]);
    }

    #[test]
    fn test_redo_delete_after_undo() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.delete_bytes(1, 1).unwrap();
        doc.undo();
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03]);

        doc.redo();
        assert_eq!(doc.to_vec(), vec![0x01, 0x03]);
    }

    // =========================================
    // Edge cases: new edit after undo clears redo
    // =========================================

    #[test]
    fn test_insert_after_undo_clears_redo() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(0, 0xFF).unwrap();
        doc.undo();

        doc.insert_bytes(0, &[0xAA]).unwrap();
        assert!(!doc.can_redo());
        assert_eq!(doc.get_byte(0), Some(0xAA));
    }

    #[test]
    fn test_delete_after_undo_clears_redo() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(0, 0xFF).unwrap();
        doc.undo();

        doc.delete_bytes(0, 1).unwrap();
        assert!(!doc.can_redo());
        assert_eq!(doc.to_vec(), vec![0x02, 0x03]);
    }

    // =========================================
    // Edge cases: rapid sequential edits (piece fragmentation)
    // =========================================

    #[test]
    fn test_many_single_byte_edits() {
        let mut doc = Document::with_data(vec![0x00; 256]);
        // Set every byte to its index
        for i in 0..256 {
            doc.set_byte(i, i as u8).unwrap();
        }
        let expected: Vec<u8> = (0..=255).collect();
        assert_eq!(doc.to_vec(), expected);
    }

    #[test]
    fn test_many_single_byte_inserts_at_beginning() {
        let mut doc = Document::new();
        // Insert bytes in reverse order at position 0
        for i in (0u8..50).rev() {
            doc.insert_bytes(0, &[i]).unwrap();
        }
        let expected: Vec<u8> = (0..50).collect();
        assert_eq!(doc.to_vec(), expected);
    }

    #[test]
    fn test_many_single_byte_inserts_at_end() {
        let mut doc = Document::new();
        for i in 0u8..50 {
            doc.insert_bytes(doc.len(), &[i]).unwrap();
        }
        let expected: Vec<u8> = (0..50).collect();
        assert_eq!(doc.to_vec(), expected);
    }

    #[test]
    fn test_alternating_insert_delete() {
        let mut doc = Document::with_data(vec![0x00; 10]);
        // Insert then delete repeatedly
        for _ in 0..20 {
            doc.insert_bytes(5, &[0xFF]).unwrap();
            doc.delete_bytes(5, 1).unwrap();
        }
        assert_eq!(doc.len(), 10);
        assert_eq!(doc.to_vec(), vec![0x00; 10]);
    }

    // =========================================
    // Edge cases: operations after delete-all
    // =========================================

    #[test]
    fn test_insert_after_delete_all() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.delete_bytes(0, 3).unwrap();
        assert!(doc.is_empty());

        doc.insert_bytes(0, &[0xAA, 0xBB]).unwrap();
        assert_eq!(doc.to_vec(), vec![0xAA, 0xBB]);
    }

    #[test]
    fn test_set_byte_after_delete_all_fails() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.delete_bytes(0, 3).unwrap();
        assert!(doc.set_byte(0, 0xFF).is_err());
    }

    // =========================================
    // Edge cases: get_slice across complex piece boundaries
    // =========================================

    #[test]
    fn test_get_slice_single_byte() {
        let doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        assert_eq!(doc.get_slice(1..2), Some(vec![0x02]));
    }

    #[test]
    fn test_get_slice_after_multiple_edits() {
        let mut doc = Document::with_data(vec![0x00; 20]);
        doc.set_byte(5, 0xAA).unwrap();
        doc.set_byte(10, 0xBB).unwrap();
        doc.insert_bytes(8, &[0xCC, 0xDD]).unwrap();
        // Original: 00*20
        // After set(5, AA): 00 00 00 00 00 AA 00 00 00 00 00 00 00 00 00 00 00 00 00 00
        // After set(10, BB): 00 00 00 00 00 AA 00 00 00 00 BB 00 00 00 00 00 00 00 00 00
        // After insert(8, CC DD): 00 00 00 00 00 AA 00 00 CC DD 00 00 BB 00 ...
        let full = doc.to_vec();
        assert_eq!(doc.len(), 22);

        // Verify slices match full vector
        for start in 0..doc.len() {
            for end in start..=doc.len() {
                assert_eq!(
                    doc.get_slice(start..end),
                    Some(full[start..end].to_vec()),
                    "Mismatch at slice {}..{}",
                    start,
                    end
                );
            }
        }
    }

    // =========================================
    // Edge cases: replace_bytes boundary conditions
    // =========================================

    #[test]
    fn test_replace_at_beginning() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.replace_bytes(0, 2, &[0xAA, 0xBB, 0xCC]).unwrap();
        assert_eq!(doc.to_vec(), vec![0xAA, 0xBB, 0xCC, 0x03, 0x04, 0x05]);
    }

    #[test]
    fn test_replace_at_end() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.replace_bytes(3, 2, &[0xAA]).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03, 0xAA]);
    }

    #[test]
    fn test_replace_entire_document() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.replace_bytes(0, 3, &[0xAA, 0xBB]).unwrap();
        assert_eq!(doc.to_vec(), vec![0xAA, 0xBB]);
        assert_eq!(doc.len(), 2);
    }

    #[test]
    fn test_replace_with_empty_is_delete() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.replace_bytes(1, 3, &[]).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x05]);
    }

    #[test]
    fn test_replace_zero_count_is_insert() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.replace_bytes(1, 0, &[0xAA, 0xBB]).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0xAA, 0xBB, 0x02, 0x03]);
    }

    #[test]
    fn test_replace_out_of_bounds() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        assert!(doc.replace_bytes(2, 5, &[0xFF]).is_err());
    }

    #[test]
    fn test_replace_undo_redo() {
        let original = vec![0x01, 0x02, 0x03, 0x04, 0x05];
        let mut doc = Document::with_data(original.clone());
        doc.replace_bytes(1, 2, &[0xAA, 0xBB, 0xCC]).unwrap();
        let replaced = doc.to_vec();

        doc.undo();
        assert_eq!(doc.to_vec(), original);

        doc.redo();
        assert_eq!(doc.to_vec(), replaced);
    }

    // =========================================
    // Edge cases: has_unsaved_changes tracking
    // =========================================

    #[test]
    fn test_unsaved_changes_after_insert() {
        let mut doc = Document::with_data(vec![0x01]);
        assert!(!doc.has_unsaved_changes());
        doc.insert_bytes(0, &[0x00]).unwrap();
        assert!(doc.has_unsaved_changes());
    }

    #[test]
    fn test_unsaved_changes_after_delete() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        assert!(!doc.has_unsaved_changes());
        doc.delete_bytes(0, 1).unwrap();
        assert!(doc.has_unsaved_changes());
    }

    #[test]
    fn test_unsaved_changes_false_after_undo_all_edits() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        doc.set_byte(0, 0xFF).unwrap();
        doc.insert_bytes(0, &[0xAA]).unwrap();
        doc.delete_bytes(2, 1).unwrap();

        // Undo all three operations
        doc.undo();
        doc.undo();
        doc.undo();
        assert!(!doc.has_unsaved_changes());
    }

    #[test]
    fn test_unsaved_changes_true_after_partial_undo() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        doc.set_byte(0, 0xAA).unwrap();
        doc.set_byte(1, 0xBB).unwrap();

        doc.undo(); // undo second edit
        assert!(doc.has_unsaved_changes()); // first edit still there
    }

    // =========================================
    // Edge cases: undo stack limit
    // =========================================

    #[test]
    fn test_undo_stack_limit() {
        let mut doc = Document::with_data(vec![0x00]);
        // Default max_undo_levels is 1000
        // Perform more edits than the limit
        for i in 0..1010u16 {
            doc.set_byte(0, (i % 256) as u8).unwrap();
        }
        // Should be able to undo up to max_undo_levels, not more
        let mut undo_count = 0;
        while doc.undo().is_some() {
            undo_count += 1;
        }
        assert_eq!(undo_count, 1000);
    }

    // =========================================
    // Edge cases: can_undo / can_redo states
    // =========================================

    #[test]
    fn test_can_undo_redo_initial_state() {
        let doc = Document::new();
        assert!(!doc.can_undo());
        assert!(!doc.can_redo());
    }

    #[test]
    fn test_can_undo_after_edit() {
        let mut doc = Document::with_data(vec![0x01]);
        doc.set_byte(0, 0xFF).unwrap();
        assert!(doc.can_undo());
        assert!(!doc.can_redo());
    }

    #[test]
    fn test_can_redo_after_undo() {
        let mut doc = Document::with_data(vec![0x01]);
        doc.set_byte(0, 0xFF).unwrap();
        doc.undo();
        assert!(!doc.can_undo());
        assert!(doc.can_redo());
    }

    // =========================================
    // Edge cases: is_modified tracking
    // =========================================

    #[test]
    fn test_is_modified_untouched_byte() {
        let doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        assert!(!doc.is_modified(0));
        assert!(!doc.is_modified(1));
        assert!(!doc.is_modified(2));
    }

    #[test]
    fn test_is_modified_after_set() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(1, 0xFF).unwrap();
        assert!(!doc.is_modified(0));
        assert!(doc.is_modified(1));
        assert!(!doc.is_modified(2));
    }

    #[test]
    fn test_is_modified_after_insert() {
        let mut doc = Document::with_data(vec![0x01, 0x03]);
        doc.insert_bytes(1, &[0x02]).unwrap();
        // Inserted byte should be "modified"
        assert!(!doc.is_modified(0));
        assert!(doc.is_modified(1));
        assert!(!doc.is_modified(2));
    }

    #[test]
    fn test_is_modified_out_of_bounds() {
        let doc = Document::with_data(vec![0x01]);
        assert!(!doc.is_modified(100));
    }

    // =========================================
    // Edge cases: coalesce_pieces
    // =========================================

    #[test]
    fn test_coalesce_empty_pieces() {
        let mut doc = Document::new();
        doc.coalesce_pieces(); // should not panic
        assert_eq!(doc.pieces.len(), 0);
    }

    #[test]
    fn test_coalesce_single_piece() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.coalesce_pieces();
        assert_eq!(doc.pieces.len(), 1);
    }

    #[test]
    fn test_coalesce_non_adjacent_same_source() {
        let mut doc = Document::new();
        // Two pieces from same source but not contiguous
        doc.pieces = vec![
            Piece {
                source: PieceSource::Original,
                start: 0,
                length: 3,
            },
            Piece {
                source: PieceSource::Original,
                start: 5, // gap: not adjacent to 0+3=3
                length: 3,
            },
        ];
        doc.coalesce_pieces();
        assert_eq!(doc.pieces.len(), 2); // should NOT merge
    }

    // =========================================
    // Edge cases: write_to with fragmented pieces
    // =========================================

    #[test]
    fn test_write_to_after_heavy_fragmentation() {
        let mut doc = Document::with_data(vec![0x00; 100]);
        // Create lots of piece splits
        for i in (0..100).step_by(3) {
            doc.set_byte(i, 0xFF).unwrap();
        }
        doc.insert_bytes(50, &[0xAA; 10]).unwrap();
        doc.delete_bytes(20, 5).unwrap();

        // write_to should always match to_vec
        let mut buf = Vec::new();
        doc.write_to(&mut buf).unwrap();
        assert_eq!(buf, doc.to_vec());
    }

    #[test]
    fn test_write_range_to_after_fragmentation() {
        let mut doc = Document::with_data(vec![0x00; 50]);
        for i in 0..50 {
            doc.set_byte(i, i as u8).unwrap();
        }
        let full = doc.to_vec();

        // Test various sub-ranges
        for &(start, end) in &[(0, 10), (10, 30), (25, 50), (0, 50), (49, 50)] {
            let mut buf = Vec::new();
            doc.write_range_to(start..end, &mut buf).unwrap();
            assert_eq!(buf, full[start..end].to_vec(), "Range {}..{}", start, end);
        }
    }

    // =========================================
    // Edge cases: undo returns correct offset
    // =========================================

    #[test]
    fn test_undo_returns_edit_offset() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);

        doc.set_byte(3, 0xFF).unwrap();
        assert_eq!(doc.undo(), Some(3));

        doc.insert_bytes(1, &[0xAA]).unwrap();
        assert_eq!(doc.undo(), Some(1));

        doc.delete_bytes(2, 2).unwrap();
        assert_eq!(doc.undo(), Some(2));
    }

    #[test]
    fn test_redo_returns_edit_offset() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.set_byte(2, 0xFF).unwrap();
        doc.undo();
        assert_eq!(doc.redo(), Some(2));
    }

    // =========================================
    // Edge cases: large data operations
    // =========================================

    #[test]
    fn test_large_insert() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        let large_data = vec![0xFF; 10_000];
        doc.insert_bytes(1, &large_data).unwrap();

        assert_eq!(doc.len(), 10_002);
        assert_eq!(doc.get_byte(0), Some(0x01));
        assert_eq!(doc.get_byte(1), Some(0xFF));
        assert_eq!(doc.get_byte(10_000), Some(0xFF));
        assert_eq!(doc.get_byte(10_001), Some(0x02));
    }

    #[test]
    fn test_large_document_random_access() {
        let size = 100_000;
        let data: Vec<u8> = (0..size).map(|i| (i % 256) as u8).collect();
        let doc = Document::with_data(data.clone());

        // Random access pattern (not sequential)
        let positions = [0, 99_999, 50_000, 1, 99_998, 12_345, 67_890];
        for &pos in &positions {
            assert_eq!(
                doc.get_byte(pos),
                Some((pos % 256) as u8),
                "Mismatch at position {}",
                pos
            );
        }
    }

    #[test]
    fn test_large_document_sequential_then_random() {
        let data: Vec<u8> = (0..10_000).map(|i| (i % 256) as u8).collect();
        let mut doc = Document::with_data(data);

        // Create some fragmentation
        doc.set_byte(5000, 0xFF).unwrap();
        doc.insert_bytes(2500, &[0xAA, 0xBB]).unwrap();

        // Sequential read (warms cache)
        for i in 0..doc.len() {
            assert!(doc.get_byte(i).is_some());
        }

        // Random access (cache should handle gracefully)
        assert!(doc.get_byte(9999).is_some());
        assert!(doc.get_byte(0).is_some());
        assert!(doc.get_byte(5001).is_some());
    }

    // =========================================
    // Edge cases: delete_bytes spanning multiple pieces
    // =========================================

    #[test]
    fn test_delete_spanning_three_pieces() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        // Create 3 pieces: set byte 1 and byte 3
        doc.set_byte(1, 0xAA).unwrap();
        doc.set_byte(3, 0xBB).unwrap();
        // pieces: [orig:0], [add:AA], [orig:2], [add:BB], [orig:4]

        // Delete across all modified pieces (bytes 0..4)
        doc.delete_bytes(0, 4).unwrap();
        assert_eq!(doc.to_vec(), vec![0x05]);
    }

    #[test]
    fn test_delete_partial_piece_left() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.delete_bytes(0, 3).unwrap();
        assert_eq!(doc.to_vec(), vec![0x04, 0x05]);
    }

    #[test]
    fn test_delete_partial_piece_right() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03, 0x04, 0x05]);
        doc.delete_bytes(3, 2).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x02, 0x03]);
    }

    #[test]
    fn test_delete_single_byte_from_middle() {
        let mut doc = Document::with_data(vec![0x01, 0x02, 0x03]);
        doc.delete_bytes(1, 1).unwrap();
        assert_eq!(doc.to_vec(), vec![0x01, 0x03]);
    }

    // =========================================
    // Edge cases: complex operation sequences
    // =========================================

    #[test]
    fn test_insert_set_delete_undo_all_redo_all() {
        let original = vec![0x01, 0x02, 0x03];
        let mut doc = Document::with_data(original.clone());

        doc.insert_bytes(1, &[0xAA, 0xBB]).unwrap(); // [01, AA, BB, 02, 03]
        let after_insert = doc.to_vec();

        doc.set_byte(0, 0xFF).unwrap(); // [FF, AA, BB, 02, 03]
        let after_set = doc.to_vec();

        doc.delete_bytes(3, 2).unwrap(); // [FF, AA, BB]
        let after_delete = doc.to_vec();

        // Undo step by step
        doc.undo();
        assert_eq!(doc.to_vec(), after_set);

        doc.undo();
        assert_eq!(doc.to_vec(), after_insert);

        doc.undo();
        assert_eq!(doc.to_vec(), original);

        // Redo step by step
        doc.redo();
        assert_eq!(doc.to_vec(), after_insert);

        doc.redo();
        assert_eq!(doc.to_vec(), after_set);

        doc.redo();
        assert_eq!(doc.to_vec(), after_delete);
    }

    #[test]
    fn test_empty_insert_is_noop() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        doc.insert_bytes(1, &[]).unwrap();
        assert_eq!(doc.len(), 2);
        assert!(!doc.has_unsaved_changes());
        assert!(!doc.can_undo());
    }

    #[test]
    fn test_delete_zero_is_noop() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        doc.delete_bytes(1, 0).unwrap();
        assert_eq!(doc.len(), 2);
        assert!(!doc.has_unsaved_changes());
        assert!(!doc.can_undo());
    }

    #[test]
    fn test_replace_empty_by_empty_is_noop() {
        let mut doc = Document::with_data(vec![0x01, 0x02]);
        doc.replace_bytes(1, 0, &[]).unwrap();
        assert_eq!(doc.len(), 2);
        assert!(!doc.has_unsaved_changes());
        assert!(!doc.can_undo());
    }
}
