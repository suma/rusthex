//! Cursor movement and navigation functionality
//!
//! This module contains methods for cursor movement within the hex editor:
//! - Basic directional movement (left, right, up, down)
//! - Page navigation (Page Up, Page Down)
//! - Line navigation (Home, End)
//! - Document navigation (Ctrl+Home, Ctrl+End)

use crate::HexEditor;
use crate::ui::{self, EditMode, HexNibble};
use gpui::{Point, px};

impl HexEditor {
    /// Calculate new position from current position with relative offset.
    /// Handles boundary checks to prevent underflow/overflow.
    /// Based on suma/hex cursor.cpp getRelativePosition.
    pub fn get_relative_position(&self, relative_pos: i64) -> usize {
        let current_pos = self.tab().cursor_position;
        let doc_len = self.tab().document.len();
        // In Insert mode, allow one position past the end of the document (for appending)
        let max_pos = if self.tab().edit_mode == EditMode::Insert {
            doc_len
        } else {
            doc_len.saturating_sub(1)
        };

        if relative_pos < 0 {
            // Moving backward
            let diff = relative_pos.unsigned_abs() as usize;
            if current_pos < diff {
                0
            } else {
                current_pos - diff
            }
        } else {
            // Moving forward
            let diff = relative_pos as usize;
            if current_pos.checked_add(diff).map_or(true, |p| p > max_pos) {
                max_pos
            } else {
                current_pos + diff
            }
        }
    }

    /// Move cursor to the specified position with common post-processing.
    /// Resets nibble state and ensures cursor visibility.
    /// Based on suma/hex cursor.cpp movePosition.
    pub fn move_position(&mut self, new_pos: usize) {
        let max_pos = if self.tab().edit_mode == EditMode::Insert {
            self.tab().document.len()
        } else {
            self.tab().document.len().saturating_sub(1)
        };
        self.tab_mut().cursor_position = new_pos.min(max_pos);
        self.tab_mut().hex_nibble = HexNibble::High;
        self.ensure_cursor_visible_by_row();
    }

    /// Ensure cursor is visible by scrolling to its row
    pub fn ensure_cursor_visible_by_row(&mut self) {
        let cursor_row = self.tab().cursor_position / self.bytes_per_row();
        let total_rows = {
            let base = ui::row_count(self.tab().document.len(), self.bytes_per_row());
            if self.tab().edit_mode == EditMode::Insert {
                let max_row = self.tab().document.len() / self.bytes_per_row();
                base.max(max_row + 1)
            } else {
                base
            }
        };
        let current_offset = self.tab().scroll_handle.offset();

        // If cursor is at position 0, reset scroll to top
        if self.tab().cursor_position == 0 {
            let new_offset = Point::new(current_offset.x, px(0.0));
            self.tab_mut().scroll_handle.set_offset(new_offset);
            self.tab_mut().scroll_offset = px(0.0);
            return;
        }

        if let Some(new_y_offset) = ui::calculate_scroll_to_row(
            cursor_row,
            current_offset.y,
            self.content_view_rows,
            total_rows,
            self.row_height(),
        ) {
            let new_offset = Point::new(current_offset.x, new_y_offset);
            self.tab_mut().scroll_handle.set_offset(new_offset);
            // Keep scroll_offset in sync for mouse drag calculations
            self.tab_mut().scroll_offset = new_y_offset;
        }
    }

    /// Move cursor left by one byte
    pub fn move_cursor_left(&mut self) {
        let new_pos = self.get_relative_position(-1);
        if new_pos != self.tab().cursor_position {
            self.move_position(new_pos);
        }
    }

    /// Move cursor right by one byte
    pub fn move_cursor_right(&mut self) {
        let new_pos = self.get_relative_position(1);
        if new_pos != self.tab().cursor_position {
            self.move_position(new_pos);
        }
    }

    /// Move cursor up by one row
    pub fn move_cursor_up(&mut self) {
        let offset = -(self.bytes_per_row() as i64);
        let new_pos = self.get_relative_position(offset);
        if new_pos != self.tab().cursor_position {
            self.move_position(new_pos);
        }
    }

    /// Move cursor down by one row
    pub fn move_cursor_down(&mut self) {
        let offset = self.bytes_per_row() as i64;
        let new_pos = self.get_relative_position(offset);
        if new_pos != self.tab().cursor_position {
            self.move_position(new_pos);
        }
    }

    /// Page Up: Move cursor up by one page
    pub fn move_cursor_page_up(&mut self, visible_rows: usize) {
        let rows_to_move = visible_rows.saturating_sub(1).max(1);
        let bytes_to_move = rows_to_move * self.bytes_per_row();
        let offset = -(bytes_to_move as i64);
        let new_pos = self.get_relative_position(offset);
        self.move_position(new_pos);
    }

    /// Page Down: Move cursor down by one page
    pub fn move_cursor_page_down(&mut self, visible_rows: usize) {
        let rows_to_move = visible_rows.saturating_sub(1).max(1);
        let bytes_to_move = rows_to_move * self.bytes_per_row();
        let new_pos = self.get_relative_position(bytes_to_move as i64);
        self.move_position(new_pos);
    }

    /// Home: Move cursor to the beginning of the current row
    pub fn move_cursor_home(&mut self) {
        let current_row = self.tab().cursor_position / self.bytes_per_row();
        let new_pos = current_row * self.bytes_per_row();
        self.move_position(new_pos);
    }

    /// End: Move cursor to the end of the current row
    pub fn move_cursor_end(&mut self) {
        let current_row = self.tab().cursor_position / self.bytes_per_row();
        let row_end = ((current_row + 1) * self.bytes_per_row())
            .min(self.tab().document.len())
            .saturating_sub(1);
        self.move_position(row_end);
    }

    /// Ctrl+Home: Move cursor to the beginning of the file
    pub fn move_cursor_file_start(&mut self) {
        self.move_position(0);
    }

    /// Ctrl+End: Move cursor to the end of the file
    pub fn move_cursor_file_end(&mut self) {
        let end_pos = self.tab().document.len().saturating_sub(1);
        self.move_position(end_pos);
    }
}
