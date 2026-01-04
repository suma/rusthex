//! Compare mode functionality for the hex editor
//!
//! This module contains methods for comparing two files side by side:
//! - Starting and exiting compare mode
//! - Tab selection for comparison
//! - Difference highlighting

use crate::tab::EditorTab;
use crate::HexEditor;

impl HexEditor {
    /// Start compare mode - show tab selection dialog
    pub fn start_compare_mode(&mut self) {
        if self.tabs.len() < 2 {
            self.save_message = Some("Compare mode requires 2 or more tabs".to_string());
            return;
        }
        self.compare_selection_visible = true;
    }

    /// Exit compare mode
    pub fn exit_compare_mode(&mut self) {
        self.compare_mode = false;
        self.compare_tab_index = None;
        self.compare_selection_visible = false;
        // Reset scroll position to ensure cursor is visible after exiting compare mode
        // This prevents scroll position mismatch when compare_row_count != row_count
        self.ensure_cursor_visible_by_row();
    }

    /// Select a tab for comparison
    pub fn select_compare_tab(&mut self, index: usize) {
        if index == self.active_tab {
            self.save_message = Some("Please select a different tab".to_string());
            return;
        }
        if index >= self.tabs.len() {
            return;
        }
        self.compare_tab_index = Some(index);
        self.compare_mode = true;
        self.compare_selection_visible = false;
        self.save_message = Some("Compare mode: Press Esc to exit".to_string());
    }

    /// Get compare tab reference
    #[allow(dead_code)]
    pub fn compare_tab(&self) -> Option<&EditorTab> {
        self.compare_tab_index.map(|idx| &self.tabs[idx])
    }
}
