use memmap2::Mmap;
use std::collections::HashMap;
use std::fs::File;
use std::path::PathBuf;

/// Data source for background search operations
/// This allows search to happen in a background thread without blocking UI
/// and without copying large files into memory
pub enum SearchDataSource {
    /// File path with overlay - background thread will re-open and mmap the file
    FilePath {
        path: PathBuf,
        overlay: HashMap<usize, u8>,
        len: usize,
    },
    /// In-memory data - already copied with overlay applied (for small files)
    InMemory(Vec<u8>),
}

/// Backend storage for document data
pub enum DataBackend {
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

impl DataBackend {
    /// Get byte at offset from the underlying storage
    pub fn get(&self, offset: usize) -> Option<u8> {
        match self {
            DataBackend::InMemory(data) => data.get(offset).copied(),
            DataBackend::MemoryMapped { mmap, .. } => mmap.get(offset).copied(),
        }
    }

    /// Get length of the data
    pub fn len(&self) -> usize {
        match self {
            DataBackend::InMemory(data) => data.len(),
            DataBackend::MemoryMapped { mmap, .. } => mmap.len(),
        }
    }

    /// Check if backend is empty
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Get slice of data
    pub fn get_slice(&self, range: std::ops::Range<usize>) -> Option<&[u8]> {
        match self {
            DataBackend::InMemory(data) => data.get(range),
            DataBackend::MemoryMapped { mmap, .. } => mmap.get(range),
        }
    }
}

/// Represents a single edit operation
#[derive(Debug, Clone)]
pub struct Edit {
    pub offset: usize,
    pub old_value: u8,
    pub new_value: u8,
}

/// Document manages binary data with support for large files
pub struct Document {
    // Backend storage (either in-memory or memory-mapped)
    backend: DataBackend,

    // Overlay for modifications (offset -> modified byte)
    // This allows editing without modifying the original file
    overlay: HashMap<usize, u8>,

    // File metadata
    file_path: Option<PathBuf>,

    // Change tracking
    has_unsaved_changes: bool,

    // Edit history for Undo/Redo
    undo_stack: Vec<Edit>,
    redo_stack: Vec<Edit>,
    max_undo_levels: usize,

    // Threshold for using memory-mapped files (e.g., 10MB)
    mmap_threshold: usize,
}

impl Document {
    /// Create a new empty document
    pub fn new() -> Self {
        Self {
            backend: DataBackend::InMemory(Vec::new()),
            overlay: HashMap::new(),
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
        let mut doc = Self::new();
        doc.backend = DataBackend::InMemory(data);
        doc
    }

    /// Load document from file
    /// Automatically chooses between in-memory and memory-mapped based on file size
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

        // Choose backend based on file size
        self.backend = if file_size > self.mmap_threshold {
            // Use memory-mapped file for large files
            let mmap = unsafe { Mmap::map(&file)? };
            DataBackend::MemoryMapped { mmap, file }
        } else {
            // Load into memory for small files
            let data = std::fs::read(&path)?;
            DataBackend::InMemory(data)
        };

        self.file_path = Some(path);
        self.overlay.clear();
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
    /// This will materialize all changes from the overlay
    pub fn save_as(&mut self, path: PathBuf) -> std::io::Result<()> {
        // Materialize all data with overlay applied
        let mut data = Vec::with_capacity(self.len());
        for i in 0..self.len() {
            data.push(self.get_byte(i).unwrap());
        }

        // Write to file
        std::fs::write(&path, &data)?;

        // Update state
        self.file_path = Some(path.clone());

        // Reload with new backend (might switch from mmap to in-memory or vice versa)
        self.load(path)?;

        Ok(())
    }

    /// Get byte at offset, checking overlay first
    pub fn get_byte(&self, offset: usize) -> Option<u8> {
        // Check overlay first
        if let Some(&byte) = self.overlay.get(&offset) {
            return Some(byte);
        }

        // Fall back to backend
        self.backend.get(offset)
    }

    /// Get slice of data (for efficient batch reads)
    /// Note: This applies overlay to each byte
    #[allow(dead_code)]
    pub fn get_slice(&self, range: std::ops::Range<usize>) -> Option<Vec<u8>> {
        let mut result = Vec::with_capacity(range.len());
        for i in range {
            result.push(self.get_byte(i)?);
        }
        Some(result)
    }

    /// Get document length
    pub fn len(&self) -> usize {
        self.backend.len()
    }

    /// Check if document is empty
    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Set byte at offset
    pub fn set_byte(&mut self, offset: usize, value: u8) -> Result<(), String> {
        if offset >= self.len() {
            return Err(format!("Offset {} out of bounds (size: {})", offset, self.len()));
        }

        let old_value = self.get_byte(offset).unwrap();

        if old_value != value {
            // Record edit for undo
            let edit = Edit {
                offset,
                old_value,
                new_value: value,
            };
            self.undo_stack.push(edit);

            // Limit undo stack size
            if self.undo_stack.len() > self.max_undo_levels {
                self.undo_stack.remove(0);
            }

            // Clear redo stack on new edit
            self.redo_stack.clear();

            // Apply change to overlay
            self.overlay.insert(offset, value);
            self.has_unsaved_changes = true;
        }

        Ok(())
    }

    /// Undo last edit. Returns the offset of the undone edit.
    pub fn undo(&mut self) -> Option<usize> {
        if let Some(edit) = self.undo_stack.pop() {
            let offset = edit.offset;

            // Apply reverse edit
            self.overlay.insert(edit.offset, edit.old_value);

            // Move to redo stack
            self.redo_stack.push(edit);

            // Update unsaved flag
            self.has_unsaved_changes = !self.undo_stack.is_empty() || !self.overlay.is_empty();

            Some(offset)
        } else {
            None
        }
    }

    /// Redo previously undone edit. Returns the offset of the redone edit.
    pub fn redo(&mut self) -> Option<usize> {
        if let Some(edit) = self.redo_stack.pop() {
            let offset = edit.offset;

            // Apply edit
            self.overlay.insert(edit.offset, edit.new_value);

            // Move to undo stack
            self.undo_stack.push(edit);

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

    /// Get number of modified bytes
    #[allow(dead_code)]
    pub fn modified_count(&self) -> usize {
        self.overlay.len()
    }

    /// Check if byte at offset is modified
    #[allow(dead_code)]
    pub fn is_modified(&self, offset: usize) -> bool {
        self.overlay.contains_key(&offset)
    }

    /// Efficiently copy all document data with overlay applied
    /// This is optimized for large files by copying the backend data in bulk
    /// and then applying overlay modifications
    pub fn to_vec(&self) -> Vec<u8> {
        let len = self.len();
        let mut result = Vec::with_capacity(len);

        // Copy backend data in bulk (very fast, especially for mmap)
        match &self.backend {
            DataBackend::InMemory(vec) => {
                result.extend_from_slice(vec);
            }
            DataBackend::MemoryMapped { mmap, .. } => {
                result.extend_from_slice(&mmap[..]);
            }
        }

        // Apply overlay modifications
        for (&offset, &byte) in &self.overlay {
            if offset < len {
                result[offset] = byte;
            }
        }

        result
    }

    /// Prepare data source for background search
    ///
    /// For memory-mapped files: returns file path and overlay (no copy, fast)
    /// For in-memory files: returns copied data with overlay applied (small files)
    ///
    /// This method is fast and doesn't block the UI thread.
    pub fn prepare_search_data(&self) -> SearchDataSource {
        match &self.backend {
            DataBackend::MemoryMapped { .. } => {
                // For mmap files, just pass the path and overlay
                // The background thread will re-open the file
                if let Some(path) = &self.file_path {
                    SearchDataSource::FilePath {
                        path: path.clone(),
                        overlay: self.overlay.clone(),
                        len: self.len(),
                    }
                } else {
                    // Fallback: shouldn't happen, but handle gracefully
                    SearchDataSource::InMemory(self.to_vec())
                }
            }
            DataBackend::InMemory(data) => {
                // For small in-memory files, copy with overlay applied
                let mut result = data.clone();
                for (&offset, &byte) in &self.overlay {
                    if offset < result.len() {
                        result[offset] = byte;
                    }
                }
                SearchDataSource::InMemory(result)
            }
        }
    }
}

impl Default for Document {
    fn default() -> Self {
        Self::new()
    }
}
