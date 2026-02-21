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
    pub fn set_byte(&mut self, offset: usize, value: u8) -> Result<(), String> {
        if offset >= self.cached_length {
            return Err(format!(
                "Offset {} out of bounds (size: {})",
                offset, self.cached_length
            ));
        }

        let old_value = self.get_byte(offset).unwrap();

        if old_value != value {
            // Save snapshot for undo
            let undo_record = UndoRecord {
                operation: EditOperation::Replace {
                    offset,
                    old_bytes: vec![old_value],
                    new_bytes: vec![value],
                },
                pieces_snapshot: self.pieces.clone(),
                cached_length: self.cached_length,
            };
            self.undo_stack.push(undo_record);
            if self.undo_stack.len() > self.max_undo_levels {
                self.undo_stack.remove(0);
            }
            self.redo_stack.clear();

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

        // Save snapshot for undo
        let undo_record = UndoRecord {
            operation: EditOperation::Insert {
                offset,
                data: data.to_vec(),
            },
            pieces_snapshot: self.pieces.clone(),
            cached_length: self.cached_length,
        };
        self.undo_stack.push(undo_record);
        if self.undo_stack.len() > self.max_undo_levels {
            self.undo_stack.remove(0);
        }
        self.redo_stack.clear();

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

        // Save snapshot for undo
        let undo_record = UndoRecord {
            operation: EditOperation::Delete {
                offset,
                deleted_bytes,
            },
            pieces_snapshot: self.pieces.clone(),
            cached_length: self.cached_length,
        };
        self.undo_stack.push(undo_record);
        if self.undo_stack.len() > self.max_undo_levels {
            self.undo_stack.remove(0);
        }
        self.redo_stack.clear();

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

        // Save snapshot for undo
        let undo_record = UndoRecord {
            operation: EditOperation::Replace {
                offset,
                old_bytes,
                new_bytes: data.to_vec(),
            },
            pieces_snapshot: self.pieces.clone(),
            cached_length: self.cached_length,
        };
        self.undo_stack.push(undo_record);
        if self.undo_stack.len() > self.max_undo_levels {
            self.undo_stack.remove(0);
        }
        self.redo_stack.clear();

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

    /// Get file name
    pub fn file_name(&self) -> Option<&str> {
        self.file_path
            .as_ref()
            .and_then(|p| p.file_name())
            .and_then(|n| n.to_str())
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
}
