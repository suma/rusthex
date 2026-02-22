//! Pattern language integration module
//!
//! Provides integration between the pattern-lang crate and the hex editor,
//! including a DataSource adapter for Document, a filesystem IncludeResolver,
//! and per-tab pattern state management.

use std::collections::HashSet;
use std::path::{Path, PathBuf};

use gpui::ScrollHandle;
use pattern_lang::error::EvalError;
use pattern_lang::preprocessor::IncludeResolver;
use pattern_lang::{DataSource, PatternEngine, PatternNode, PatternValue};

use crate::document::Document;

/// Adapter to use Document (piece table) as a pattern-lang DataSource
pub struct DocumentDataSource<'a> {
    document: &'a Document,
}

impl<'a> DocumentDataSource<'a> {
    pub fn new(document: &'a Document) -> Self {
        Self { document }
    }
}

impl DataSource for DocumentDataSource<'_> {
    fn read_bytes(&self, offset: u64, size: u64) -> Result<Vec<u8>, EvalError> {
        let start = offset as usize;
        let end = start + size as usize;
        if end > self.document.len() {
            return Err(EvalError::new(format!(
                "read out of bounds: offset={}, size={}, data_len={}",
                offset,
                size,
                self.document.len()
            )));
        }
        match self.document.get_slice(start..end) {
            Some(data) => Ok(data),
            None => Err(EvalError::new(format!(
                "failed to read bytes at offset={}, size={}",
                offset, size
            ))),
        }
    }

    fn size(&self) -> u64 {
        self.document.len() as u64
    }
}

/// Filesystem-based IncludeResolver for .hexpat #include and import directives.
/// Searches multiple directories in order to find the requested file.
pub struct FileSystemResolver {
    search_dirs: Vec<PathBuf>,
}

impl FileSystemResolver {
    pub fn new(search_dirs: Vec<PathBuf>) -> Self {
        Self { search_dirs }
    }
}

impl IncludeResolver for FileSystemResolver {
    fn resolve(&self, path: &str, _is_system: bool) -> Option<String> {
        for dir in &self.search_dirs {
            let full_path = dir.join(path);
            if let Ok(content) = std::fs::read_to_string(&full_path) {
                return Some(content);
            }
        }
        None
    }
}

/// Information about an available .hexpat file
pub struct PatternFileInfo {
    /// File stem (e.g., "png")
    pub name: String,
    /// Full path to the .hexpat file
    pub path: PathBuf,
}

/// Result of pattern evaluation
pub enum PatternResult {
    Ok(Vec<PatternNode>),
    Err(PatternError),
}

/// Error information from pattern evaluation
pub struct PatternError {
    /// Path to the .hexpat file that failed
    pub path: PathBuf,
    /// Error message
    pub message: String,
}

/// Per-tab pattern state
pub struct PatternState {
    /// Available .hexpat files in the configured directory
    pub available_patterns: Vec<PatternFileInfo>,
    /// Currently selected pattern index (None = no pattern selected)
    pub selected_index: Option<usize>,
    /// Evaluation result (success or error)
    pub result: Option<PatternResult>,
    /// Tree view scroll state
    pub scroll_handle: ScrollHandle,
    /// Expanded tree nodes (by dot-separated path, e.g. "header.magic")
    pub expanded_nodes: HashSet<String>,
}

impl PatternState {
    pub fn new() -> Self {
        Self {
            available_patterns: Vec::new(),
            selected_index: None,
            result: None,
            scroll_handle: ScrollHandle::new(),
            expanded_nodes: HashSet::new(),
        }
    }
}

impl Default for PatternState {
    fn default() -> Self {
        Self::new()
    }
}

/// Scan a directory for .hexpat files
pub fn scan_hexpat_dir(dir: &str) -> Vec<PatternFileInfo> {
    if dir.is_empty() {
        return Vec::new();
    }
    let dir_path = Path::new(dir);
    if !dir_path.is_dir() {
        return Vec::new();
    }
    let mut patterns = Vec::new();
    if let Ok(entries) = std::fs::read_dir(dir_path) {
        for entry in entries.flatten() {
            let path = entry.path();
            if path.extension().and_then(|e| e.to_str()) == Some("hexpat") {
                if let Some(stem) = path.file_stem().and_then(|s| s.to_str()) {
                    patterns.push(PatternFileInfo {
                        name: stem.to_string(),
                        path,
                    });
                }
            }
        }
    }
    patterns.sort_by(|a, b| a.name.cmp(&b.name));
    patterns
}

/// Run a pattern against a document and return the result.
///
/// `include_dirs` specifies additional directories to search for #include / import files.
/// The search order is: include_dirs → hexpat_dir sibling "includes/" → hexpat_dir itself.
pub fn evaluate_pattern(
    pattern_path: &Path,
    document: &Document,
    hexpat_dir: &Path,
    include_dirs: &[String],
) -> PatternResult {
    let source = match std::fs::read_to_string(pattern_path) {
        Ok(s) => s,
        Err(e) => {
            return PatternResult::Err(PatternError {
                path: pattern_path.to_path_buf(),
                message: format!("Failed to read pattern file: {}", e),
            });
        }
    };

    // Build search directories with deduplication
    let mut search_dirs = Vec::new();
    let mut seen = std::collections::HashSet::new();

    // 1. User-configured include_dirs (highest priority)
    for dir in include_dirs {
        let path = PathBuf::from(dir);
        if path.is_dir() {
            if seen.insert(path.clone()) {
                search_dirs.push(path);
            }
        }
    }

    // 2. Auto-detected sibling "includes/" directory
    if let Some(includes_dir) = hexpat_dir.parent().map(|p| p.join("includes")) {
        if includes_dir.is_dir() && seen.insert(includes_dir.clone()) {
            search_dirs.push(includes_dir);
        }
    }

    // 3. hexpat_dir itself
    let hexpat_path = hexpat_dir.to_path_buf();
    if seen.insert(hexpat_path.clone()) {
        search_dirs.push(hexpat_path);
    }

    let resolver = FileSystemResolver::new(search_dirs);
    let engine = PatternEngine::with_resolver(resolver);
    let data_source = DocumentDataSource::new(document);

    match engine.run(&source, &data_source) {
        Ok(nodes) => PatternResult::Ok(nodes),
        Err(e) => PatternResult::Err(PatternError {
            path: pattern_path.to_path_buf(),
            message: format!("{}", e),
        }),
    }
}

/// Format a PatternValue for display
pub fn format_value(value: &PatternValue) -> String {
    match value {
        PatternValue::Unsigned(v) => {
            if *v <= 0xFF {
                format!("{} (0x{:02X})", v, v)
            } else if *v <= 0xFFFF {
                format!("{} (0x{:04X})", v, v)
            } else if *v <= 0xFFFF_FFFF {
                format!("{} (0x{:08X})", v, v)
            } else {
                format!("{} (0x{:X})", v, v)
            }
        }
        PatternValue::Signed(v) => format!("{}", v),
        PatternValue::Float(v) => format!("{:.6}", v),
        PatternValue::Bool(v) => format!("{}", v),
        PatternValue::Char(v) => format!("'{}'", v),
        PatternValue::String(v) => format!("\"{}\"", v),
        PatternValue::Enum { value, name } => format!("{} ({})", name, value),
        PatternValue::Array => "[ ... ]".to_string(),
        PatternValue::Struct => "{ ... }".to_string(),
        PatternValue::Union => "{ ... }".to_string(),
        PatternValue::Bitfield => "{ ... }".to_string(),
        PatternValue::Pointer { address } => format!("*0x{:08X}", address),
        PatternValue::Padding(size) => format!("padding[{}]", size),
        PatternValue::LazyStructArray {
            type_name, count, ..
        } => format!("[{} {}]", count, type_name),
        PatternValue::BulkData {
            data, elem_type, ..
        } => {
            let elem_size = elem_type.size().unwrap_or(1) as usize;
            let count = if elem_size > 0 {
                data.len() / elem_size
            } else {
                0
            };
            format!("[{} elements]", count)
        }
    }
}

/// Count total visible nodes in a tree (for scroll height calculation)
pub fn count_visible_nodes(
    nodes: &[PatternNode],
    expanded: &HashSet<String>,
    prefix: &str,
) -> usize {
    let mut count = 0;
    for node in nodes {
        if node.attributes.hidden {
            continue;
        }
        count += 1;
        let path = if prefix.is_empty() {
            node.name.clone()
        } else {
            format!("{}.{}", prefix, node.name)
        };
        if !node.children.is_empty() && expanded.contains(&path) {
            count += count_visible_nodes(&node.children, expanded, &path);
        }
    }
    count
}
