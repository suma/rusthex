//! Bookmark functionality for the hex editor
//!
//! This module contains methods for managing bookmarks:
//! - Toggle bookmark at current position
//! - Navigate between bookmarks
//! - Clear all bookmarks

use crate::HexEditor;

impl HexEditor {
    /// Toggle bookmark at current cursor position
    pub fn toggle_bookmark(&mut self) {
        let pos = self.tab().cursor_position;
        if self.tab().bookmarks.contains(&pos) {
            self.tab_mut().bookmarks.remove(&pos);
            self.save_message = Some(format!("Bookmark removed at 0x{:08X}", pos));
        } else {
            self.tab_mut().bookmarks.insert(pos);
            self.save_message = Some(format!("Bookmark added at 0x{:08X}", pos));
        }
    }

    /// Jump to next bookmark
    pub fn next_bookmark(&mut self) {
        let current = self.tab().cursor_position;
        let bookmarks = &self.tab().bookmarks;

        if bookmarks.is_empty() {
            self.save_message = Some("No bookmarks set".to_string());
            return;
        }

        // Find the first bookmark after current position
        if let Some(&next) = bookmarks.range((current + 1)..).next() {
            self.move_position(next);
            self.save_message = Some(format!("Jumped to bookmark at 0x{:08X}", next));
        } else {
            // Wrap around to first bookmark
            if let Some(&first) = bookmarks.iter().next() {
                self.move_position(first);
                self.save_message = Some(format!("Jumped to bookmark at 0x{:08X} (wrapped)", first));
            }
        }
    }

    /// Jump to previous bookmark
    pub fn prev_bookmark(&mut self) {
        let current = self.tab().cursor_position;
        let bookmarks = &self.tab().bookmarks;

        if bookmarks.is_empty() {
            self.save_message = Some("No bookmarks set".to_string());
            return;
        }

        // Find the last bookmark before current position
        if let Some(&prev) = bookmarks.range(..current).next_back() {
            self.move_position(prev);
            self.save_message = Some(format!("Jumped to bookmark at 0x{:08X}", prev));
        } else {
            // Wrap around to last bookmark
            if let Some(&last) = bookmarks.iter().next_back() {
                self.move_position(last);
                self.save_message = Some(format!("Jumped to bookmark at 0x{:08X} (wrapped)", last));
            }
        }
    }

    /// Clear all bookmarks
    pub fn clear_bookmarks(&mut self) {
        let count = self.tab().bookmarks.len();
        self.tab_mut().bookmarks.clear();
        self.save_message = Some(format!("Cleared {} bookmark(s)", count));
    }
}
