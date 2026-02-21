//! Editor tab module
//!
//! Contains the EditorTab struct which represents a single editor tab
//! with its own document and state.

use gpui::{Pixels, ScrollHandle, px};
use gpui_component::scroll::ScrollbarState;
use std::collections::{BTreeMap, HashSet};
use std::sync::atomic::{AtomicBool, AtomicUsize};
use std::sync::{Arc, Mutex};

use crate::document::Document;
use crate::pattern::PatternState;
use crate::render_cache::RenderCache;
use crate::search::SearchMode;
use crate::ui::{EditMode, EditPane, HexNibble};

/// Represents a single editor tab with its own document and state
pub struct EditorTab {
    pub document: Document,
    pub cursor_position: usize,
    pub scroll_handle: ScrollHandle,
    pub scrollbar_state: ScrollbarState,
    pub scroll_offset: Pixels,
    pub selection_start: Option<usize>,
    pub edit_pane: EditPane,
    pub edit_mode: EditMode,
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
    // Bookmarks (sorted map of byte offset -> comment)
    pub bookmarks: BTreeMap<usize, String>,
    // Bookmark comment editing state
    pub bookmark_comment_editing: bool,
    pub bookmark_comment_text: String,
    pub bookmark_comment_position: usize,
    // Pattern language state
    pub pattern: PatternState,
    // Logical first visible row for programmatic scroll (bypass f32 precision loss)
    pub scroll_logical_row: Option<usize>,
    // Cursor navigation history (back/forward)
    pub cursor_history: Vec<usize>,
    pub cursor_history_index: usize,
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
            edit_mode: EditMode::Overwrite,
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
            bookmarks: BTreeMap::new(),
            bookmark_comment_editing: false,
            bookmark_comment_text: String::new(),
            bookmark_comment_position: 0,
            pattern: PatternState::new(),
            scroll_logical_row: None,
            cursor_history: Vec::new(),
            cursor_history_index: 0,
        }
    }

    pub fn with_document(document: Document) -> Self {
        Self {
            document,
            ..Self::new()
        }
    }

    /// Maximum number of cursor history entries
    const MAX_CURSOR_HISTORY: usize = 1024;

    /// Push a position onto the navigation history.
    /// Discards any forward entries if not at the front.
    pub fn push_history(&mut self, pos: usize) {
        // If we're not at the front of the history, discard forward entries
        if self.cursor_history_index > 0 {
            self.cursor_history.drain(0..self.cursor_history_index);
            self.cursor_history_index = 0;
        }

        // Don't push duplicate of current front
        if self.cursor_history.first() == Some(&pos) {
            return;
        }

        self.cursor_history.insert(0, pos);

        // Enforce maximum size
        if self.cursor_history.len() > Self::MAX_CURSOR_HISTORY {
            self.cursor_history.truncate(Self::MAX_CURSOR_HISTORY);
        }
    }

    /// Navigate back in cursor history. Returns target position if available.
    pub fn history_back(&mut self) -> Option<usize> {
        if self.cursor_history_index >= self.cursor_history.len() {
            return None;
        }

        // If at front, save current position first so we can return to it
        if self.cursor_history_index == 0 {
            let pos = self.cursor_position;
            if self.cursor_history.first() != Some(&pos) {
                self.cursor_history.insert(0, pos);
            }
        }

        self.cursor_history_index += 1;
        if self.cursor_history_index >= self.cursor_history.len() {
            self.cursor_history_index = self.cursor_history.len() - 1;
        }
        Some(self.cursor_history[self.cursor_history_index])
    }

    /// Navigate forward in cursor history. Returns target position if available.
    pub fn history_forward(&mut self) -> Option<usize> {
        if self.cursor_history_index == 0 {
            return None;
        }

        self.cursor_history_index -= 1;
        Some(self.cursor_history[self.cursor_history_index])
    }

    /// Get display name for the tab
    pub fn display_name(&self) -> String {
        if let Some(name) = self.document.file_name() {
            let modified = if self.document.has_unsaved_changes() {
                "*"
            } else {
                ""
            };
            format!("{}{}", name, modified)
        } else {
            let modified = if self.document.has_unsaved_changes() {
                "*"
            } else {
                ""
            };
            format!("Untitled{}", modified)
        }
    }
}

impl Default for EditorTab {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_push_history_basic() {
        let mut tab = EditorTab::new();
        tab.push_history(100);
        tab.push_history(200);
        tab.push_history(300);
        assert_eq!(tab.cursor_history, vec![300, 200, 100]);
        assert_eq!(tab.cursor_history_index, 0);
    }

    #[test]
    fn test_push_history_no_duplicate() {
        let mut tab = EditorTab::new();
        tab.push_history(100);
        tab.push_history(100); // duplicate
        assert_eq!(tab.cursor_history, vec![100]);
    }

    #[test]
    fn test_navigate_back_and_forward() {
        let mut tab = EditorTab::new();
        tab.cursor_position = 0;
        tab.push_history(0);
        tab.cursor_position = 100;
        tab.push_history(100);
        tab.cursor_position = 200;
        tab.push_history(200);
        tab.cursor_position = 300;

        // history: [200, 100, 0], cursor at 300

        // Go back: should save current pos (300) and move to 200
        let target = tab.history_back();
        assert_eq!(target, Some(200));
        tab.cursor_position = 200;

        // Go back again: move to 100
        let target = tab.history_back();
        assert_eq!(target, Some(100));
        tab.cursor_position = 100;

        // Go forward: move to 200
        let target = tab.history_forward();
        assert_eq!(target, Some(200));
        tab.cursor_position = 200;

        // Go forward: move to 300
        let target = tab.history_forward();
        assert_eq!(target, Some(300));
        tab.cursor_position = 300;

        // Go forward again: should return None (already at front)
        let target = tab.history_forward();
        assert_eq!(target, None);
    }

    #[test]
    fn test_new_jump_clears_forward_history() {
        let mut tab = EditorTab::new();
        tab.cursor_position = 0;
        tab.push_history(0);
        tab.cursor_position = 100;
        tab.push_history(100);
        tab.cursor_position = 200;

        // Go back to 100
        let target = tab.history_back();
        assert_eq!(target, Some(100));
        tab.cursor_position = 100;

        // Now jump to a new position 500 — forward history (200) should be discarded
        tab.push_history(100); // current pos before jump
        tab.cursor_position = 500;

        // Go back: should go to 100, not 200
        let target = tab.history_back();
        assert_eq!(target, Some(100));
    }

    #[test]
    fn test_history_max_limit() {
        let mut tab = EditorTab::new();
        for i in 0..2000 {
            tab.push_history(i);
        }
        assert_eq!(tab.cursor_history.len(), EditorTab::MAX_CURSOR_HISTORY);
        // Most recent should be at front
        assert_eq!(tab.cursor_history[0], 1999);
    }

    #[test]
    fn test_navigate_back_empty_history() {
        let mut tab = EditorTab::new();
        tab.cursor_position = 42;
        let target = tab.history_back();
        assert_eq!(target, None);
    }

    #[test]
    fn test_navigate_forward_at_front() {
        let mut tab = EditorTab::new();
        tab.cursor_position = 42;
        let target = tab.history_forward();
        assert_eq!(target, None);
    }
}
