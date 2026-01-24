//! Editor tab module
//!
//! Contains the EditorTab struct which represents a single editor tab
//! with its own document and state.

use gpui::{px, Pixels, ScrollHandle};
use gpui_component::scroll::ScrollbarState;
use std::collections::{HashSet, BTreeSet};
use std::sync::atomic::{AtomicBool, AtomicUsize};
use std::sync::{Arc, Mutex};

use crate::document::Document;
use crate::render_cache::RenderCache;
use crate::search::SearchMode;
use crate::ui::{EditPane, HexNibble};

/// Represents a single editor tab with its own document and state
pub struct EditorTab {
    pub document: Document,
    pub cursor_position: usize,
    pub scroll_handle: ScrollHandle,
    pub scrollbar_state: ScrollbarState,
    pub scroll_offset: Pixels,
    pub selection_start: Option<usize>,
    pub edit_pane: EditPane,
    pub hex_nibble: HexNibble,
    // Search state per tab
    pub search_visible: bool,
    pub search_query: String,
    pub search_mode: SearchMode,
    pub search_results: Vec<usize>,
    pub current_search_index: Option<usize>,
    pub is_searching: bool,
    pub search_truncated: bool,
    pub search_cancel_flag: Arc<AtomicBool>,
    pub shared_search_results: Arc<Mutex<Option<(Vec<usize>, HashSet<usize>, bool)>>>,
    pub search_match_set: HashSet<usize>,
    pub search_progress: Arc<AtomicUsize>,
    pub search_total: usize,
    // Render cache for performance optimization
    pub render_cache: RenderCache,
    // Bookmarks (sorted set of byte offsets)
    pub bookmarks: BTreeSet<usize>,
}

impl EditorTab {
    pub fn new() -> Self {
        Self {
            document: Document::new(),
            cursor_position: 0,
            scroll_handle: ScrollHandle::new(),
            scrollbar_state: ScrollbarState::default(),
            scroll_offset: px(0.0),
            selection_start: None,
            edit_pane: EditPane::Hex,
            hex_nibble: HexNibble::High,
            search_visible: false,
            search_query: String::new(),
            search_mode: SearchMode::Ascii,
            search_results: Vec::new(),
            current_search_index: None,
            is_searching: false,
            search_truncated: false,
            search_cancel_flag: Arc::new(AtomicBool::new(false)),
            shared_search_results: Arc::new(Mutex::new(None)),
            search_match_set: HashSet::new(),
            search_progress: Arc::new(AtomicUsize::new(0)),
            search_total: 0,
            render_cache: RenderCache::new(),
            bookmarks: BTreeSet::new(),
        }
    }

    pub fn with_document(document: Document) -> Self {
        Self {
            document,
            ..Self::new()
        }
    }

    /// Get display name for the tab
    pub fn display_name(&self) -> String {
        if let Some(name) = self.document.file_name() {
            let modified = if self.document.has_unsaved_changes() { "*" } else { "" };
            format!("{}{}", name, modified)
        } else {
            let modified = if self.document.has_unsaved_changes() { "*" } else { "" };
            format!("Untitled{}", modified)
        }
    }
}

impl Default for EditorTab {
    fn default() -> Self {
        Self::new()
    }
}
