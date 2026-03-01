//! Bookmark functionality for the hex editor
//!
//! This module contains methods for managing bookmarks:
//! - Toggle bookmark at current position
//! - Navigate between bookmarks
//! - Clear all bookmarks
//! - Edit bookmark comments

use crate::HexEditor;
use crate::ui;

impl HexEditor {
    /// Format an address for status messages using the current document's address width
    fn fmt_addr(&self, offset: usize) -> String {
        let chars = ui::address_chars(self.tab().document.len());
        format!("0x{}", ui::format_address(offset, chars))
    }

    /// Toggle bookmark at current cursor position.
    /// If no bookmark exists, add one and enter comment editing mode.
    /// If a bookmark exists, remove it.
    pub fn toggle_bookmark(&mut self) {
        let pos = self.tab().cursor_position;
        if self.tab().bookmarks.contains_key(&pos) {
            self.tab_mut().bookmarks.remove(&pos);
            self.log(crate::log_panel::LogLevel::Info, format!("Bookmark removed at {}", self.fmt_addr(pos)));
        } else {
            self.tab_mut().bookmarks.insert(pos, String::new());
            // Close other input bars (mutual exclusion)
            self.tab_mut().search_visible = false;
            self.tab_mut().goto_address_visible = false;
            // Enter comment editing mode for the new bookmark
            self.tab_mut().bookmark_comment_editing = true;
            self.tab_mut().bookmark_comment_text = String::new();
            self.tab_mut().bookmark_comment_position = pos;
            self.log(crate::log_panel::LogLevel::Info, format!("Bookmark added at {} - enter comment", self.fmt_addr(pos)));
        }
    }

    /// Jump to next bookmark
    pub fn next_bookmark(&mut self) {
        let current = self.tab().cursor_position;

        if self.tab().bookmarks.is_empty() {
            self.log(crate::log_panel::LogLevel::Info, "No bookmarks set");
            return;
        }

        // Extract position and comment before mutating self
        let target = self
            .tab()
            .bookmarks
            .range((current + 1)..)
            .next()
            .map(|(&pos, comment)| (pos, comment.clone()));
        let wrapped = if target.is_none() {
            self.tab()
                .bookmarks
                .iter()
                .next()
                .map(|(&pos, comment)| (pos, comment.clone()))
        } else {
            None
        };

        if let Some((pos, comment)) = target {
            self.push_cursor_history();
            self.move_position(pos);
            if comment.is_empty() {
                self.log(crate::log_panel::LogLevel::Info, format!("Jumped to bookmark at {}", self.fmt_addr(pos)));
            } else {
                self.log(crate::log_panel::LogLevel::Info, format!("Jumped to bookmark at {}: {}", self.fmt_addr(pos), comment));
            }
        } else if let Some((pos, comment)) = wrapped {
            self.push_cursor_history();
            self.move_position(pos);
            if comment.is_empty() {
                self.log(crate::log_panel::LogLevel::Info, format!("Jumped to bookmark at {} (wrapped)", self.fmt_addr(pos)));
            } else {
                self.log(crate::log_panel::LogLevel::Info, format!(
                    "Jumped to bookmark at {}: {} (wrapped)",
                    self.fmt_addr(pos), comment
                ));
            }
        }
    }

    /// Jump to previous bookmark
    pub fn prev_bookmark(&mut self) {
        let current = self.tab().cursor_position;

        if self.tab().bookmarks.is_empty() {
            self.log(crate::log_panel::LogLevel::Info, "No bookmarks set");
            return;
        }

        // Extract position and comment before mutating self
        let target = self
            .tab()
            .bookmarks
            .range(..current)
            .next_back()
            .map(|(&pos, comment)| (pos, comment.clone()));
        let wrapped = if target.is_none() {
            self.tab()
                .bookmarks
                .iter()
                .next_back()
                .map(|(&pos, comment)| (pos, comment.clone()))
        } else {
            None
        };

        if let Some((pos, comment)) = target {
            self.push_cursor_history();
            self.move_position(pos);
            if comment.is_empty() {
                self.log(crate::log_panel::LogLevel::Info, format!("Jumped to bookmark at {}", self.fmt_addr(pos)));
            } else {
                self.log(crate::log_panel::LogLevel::Info, format!("Jumped to bookmark at {}: {}", self.fmt_addr(pos), comment));
            }
        } else if let Some((pos, comment)) = wrapped {
            self.push_cursor_history();
            self.move_position(pos);
            if comment.is_empty() {
                self.log(crate::log_panel::LogLevel::Info, format!("Jumped to bookmark at {} (wrapped)", self.fmt_addr(pos)));
            } else {
                self.log(crate::log_panel::LogLevel::Info, format!(
                    "Jumped to bookmark at {}: {} (wrapped)",
                    self.fmt_addr(pos), comment
                ));
            }
        }
    }

    /// Clear all bookmarks
    pub fn clear_bookmarks(&mut self) {
        let count = self.tab().bookmarks.len();
        self.tab_mut().bookmarks.clear();
        self.log(crate::log_panel::LogLevel::Info, format!("Cleared {} bookmark(s)", count));
    }

    /// Enter comment editing mode for the bookmark at current cursor position
    pub fn edit_bookmark_comment(&mut self) {
        let pos = self.tab().cursor_position;
        if let Some(comment) = self.tab().bookmarks.get(&pos).cloned() {
            // Close other input bars (mutual exclusion)
            self.tab_mut().search_visible = false;
            self.tab_mut().goto_address_visible = false;
            self.tab_mut().bookmark_comment_editing = true;
            self.tab_mut().bookmark_comment_text = comment;
            self.tab_mut().bookmark_comment_position = pos;
            self.log(crate::log_panel::LogLevel::Info, format!("Editing bookmark comment at {}", self.fmt_addr(pos)));
        } else {
            self.log(crate::log_panel::LogLevel::Info, "No bookmark at current position");
        }
    }

    /// Confirm and save the bookmark comment
    pub fn confirm_bookmark_comment(&mut self) {
        let pos = self.tab().bookmark_comment_position;
        let text = self.tab().bookmark_comment_text.clone();
        if self.tab().bookmarks.contains_key(&pos) {
            self.tab_mut().bookmarks.insert(pos, text.clone());
            if text.is_empty() {
                self.log(crate::log_panel::LogLevel::Info, format!("Bookmark comment cleared at {}", self.fmt_addr(pos)));
            } else {
                self.log(crate::log_panel::LogLevel::Info, format!("Bookmark comment saved at {}: {}", self.fmt_addr(pos), text));
            }
        }
        self.tab_mut().bookmark_comment_editing = false;
        self.tab_mut().bookmark_comment_text.clear();
    }

    /// Cancel bookmark comment editing
    pub fn cancel_bookmark_comment(&mut self) {
        self.tab_mut().bookmark_comment_editing = false;
        self.tab_mut().bookmark_comment_text.clear();
        self.log(crate::log_panel::LogLevel::Info, "Bookmark comment editing cancelled");
    }
}
