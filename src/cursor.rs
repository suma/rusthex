//! Cursor movement and navigation functionality
//!
//! This module contains methods for cursor movement within the hex editor:
//! - Basic directional movement (left, right, up, down)
//! - Page navigation (Page Up, Page Down)
//! - Line navigation (Home, End)
//! - Document navigation (Ctrl+Home, Ctrl+End)

use crate::HexEditor;
use crate::encoding::{DisplayChar, decode_for_display};
use crate::ui::{self, EditMode, EditPane, HexNibble};
use gpui::{Point, px};

impl HexEditor {
    /// Maximum valid cursor position, accounting for Insert mode (allows append).
    fn max_cursor_position(&self) -> usize {
        let doc_len = self.tab().document.len();
        if self.tab().edit_mode == EditMode::Insert {
            doc_len
        } else {
            doc_len.saturating_sub(1)
        }
    }

    /// Calculate new position from current position with relative offset.
    /// Handles boundary checks to prevent underflow/overflow.
    /// Based on suma/hex cursor.cpp getRelativePosition.
    pub fn get_relative_position(&self, relative_pos: i64) -> usize {
        let current_pos = self.tab().cursor_position;
        let max_pos = self.max_cursor_position();

        if relative_pos < 0 {
            // Moving backward
            let diff = relative_pos.unsigned_abs() as usize;
            current_pos.saturating_sub(diff)
        } else {
            // Moving forward
            let diff = relative_pos as usize;
            if current_pos.checked_add(diff).is_none_or(|p| p > max_pos) {
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
        self.tab_mut().cursor_position = new_pos.min(self.max_cursor_position());
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
        let visible_rows = self.content_view_rows;

        // If cursor is at position 0, reset scroll to top
        if self.tab().cursor_position == 0 {
            let new_offset = Point::new(current_offset.x, px(0.0));
            self.tab_mut().scroll_handle.set_offset(new_offset);
            self.tab_mut().scroll_offset = px(0.0);
            self.tab_mut().scroll_logical_row = Some(0);
            return;
        }

        // When scroll_logical_row is set, use it directly for visibility check
        // (bypasses f32 precision loss in offset-based visible range calculation)
        if let Some(logical_row) = self.tab().scroll_logical_row {
            let max_first_row = total_rows.saturating_sub(visible_rows);
            let first_visible = logical_row.min(max_first_row);
            let last_visible = first_visible + visible_rows.saturating_sub(1);
            if cursor_row >= first_visible && cursor_row <= last_visible {
                return; // Already visible
            }
        }

        if let Some(result) = ui::calculate_scroll_to_row(
            cursor_row,
            current_offset.y,
            visible_rows,
            total_rows,
            self.row_height(),
        ) {
            let new_offset = Point::new(current_offset.x, result.offset);
            self.tab_mut().scroll_handle.set_offset(new_offset);
            // Keep scroll_offset in sync for mouse drag calculations
            self.tab_mut().scroll_offset = result.offset;
            // Store logical row to bypass f32 precision loss in large files
            self.tab_mut().scroll_logical_row = Some(result.target_first_row);
        }
    }

    /// Move cursor by a relative byte offset, only if the position actually changes.
    fn move_cursor_by_offset(&mut self, offset: i64) {
        let new_pos = self.get_relative_position(offset);
        if new_pos != self.tab().cursor_position {
            self.move_position(new_pos);
        }
    }

    /// Move cursor left by one byte
    pub fn move_cursor_left(&mut self) {
        self.move_cursor_by_offset(-1);
    }

    /// Move cursor right by one byte
    pub fn move_cursor_right(&mut self) {
        self.move_cursor_by_offset(1);
    }

    /// Move cursor up by one row
    pub fn move_cursor_up(&mut self) {
        self.move_cursor_by_offset(-(self.bytes_per_row() as i64));
    }

    /// Move cursor down by one row
    pub fn move_cursor_down(&mut self) {
        self.move_cursor_by_offset(self.bytes_per_row() as i64);
    }

    /// Page Up: Move cursor up by one page
    pub fn move_cursor_page_up(&mut self, visible_rows: usize) {
        let bytes = visible_rows.saturating_sub(1).max(1) * self.bytes_per_row();
        self.move_position(self.get_relative_position(-(bytes as i64)));
    }

    /// Page Down: Move cursor down by one page
    pub fn move_cursor_page_down(&mut self, visible_rows: usize) {
        let bytes = visible_rows.saturating_sub(1).max(1) * self.bytes_per_row();
        self.move_position(self.get_relative_position(bytes as i64));
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

    /// Move cursor forward to the next word boundary (Vim 'w' motion).
    /// - Hex pane: 8-byte aligned boundary
    /// - ASCII pane: skip to next printable/non-printable boundary using current encoding
    pub fn move_cursor_word_forward(&mut self) {
        let current = self.tab().cursor_position;
        let max_pos = self.max_cursor_position();

        if self.tab().edit_pane == EditPane::Ascii {
            let next = self.find_word_boundary_forward(current, max_pos);
            self.move_position(next);
        } else {
            let boundary = 8;
            let next = ((current / boundary) + 1) * boundary;
            self.move_position(next.min(max_pos));
        }
    }

    /// Move cursor backward to the previous word boundary (Vim 'b' motion).
    /// - Hex pane: 8-byte aligned boundary
    /// - ASCII pane: skip to previous printable/non-printable boundary using current encoding
    pub fn move_cursor_word_backward(&mut self) {
        let current = self.tab().cursor_position;
        if current == 0 {
            return;
        }

        if self.tab().edit_pane == EditPane::Ascii {
            let prev = self.find_word_boundary_backward(current);
            self.move_position(prev);
        } else {
            let boundary = 8;
            let prev = if current.is_multiple_of(boundary) {
                current - boundary
            } else {
                (current / boundary) * boundary
            };
            self.move_position(prev);
        }
    }

    /// Find the next word boundary forward from `pos`.
    /// A "word" is a contiguous run of printable characters or a contiguous run
    /// of non-printable bytes, determined by the current text encoding.
    fn find_word_boundary_forward(&self, pos: usize, max_pos: usize) -> usize {
        let doc_len = self.tab().document.len();
        if pos >= max_pos || doc_len == 0 {
            return max_pos;
        }

        // Decode a window around the cursor (enough context for word search)
        let window_start = pos;
        let window_end = (pos + 256).min(doc_len);
        let data = self
            .tab()
            .document
            .get_slice(window_start..window_end)
            .unwrap_or_default();
        if data.is_empty() {
            return max_pos;
        }

        let chars = decode_for_display(&data, self.text_encoding);
        let is_printable = |dc: &DisplayChar| matches!(dc, DisplayChar::Char(_));

        // Current char category
        let cur_printable = is_printable(&chars[0]);

        // Skip rest of current word (same category)
        let mut i = 0;
        while i < chars.len() && is_printable(&chars[i]) == cur_printable {
            i += 1;
        }

        // Skip non-printable gap to reach next word start (skip one class boundary)
        // If we moved from printable → non-printable, we're at the start of non-printable run
        // which counts as a "word" in Vim terms, so stop here.

        (window_start + i).min(max_pos)
    }

    /// Find the previous word boundary backward from `pos`.
    fn find_word_boundary_backward(&self, pos: usize) -> usize {
        if pos == 0 {
            return 0;
        }

        let doc_len = self.tab().document.len();
        if doc_len == 0 {
            return 0;
        }

        // Decode a window behind the cursor
        let window_start = pos.saturating_sub(256);
        let window_end = pos.min(doc_len);
        let data = self
            .tab()
            .document
            .get_slice(window_start..window_end)
            .unwrap_or_default();
        if data.is_empty() {
            return 0;
        }

        let chars = decode_for_display(&data, self.text_encoding);
        let is_printable = |dc: &DisplayChar| matches!(dc, DisplayChar::Char(_));

        // pos corresponds to chars[chars.len()] (one past the window)
        // Look at the byte just before pos → chars[chars.len() - 1]
        let mut i = chars.len();

        // Category of the byte just before cursor
        let cur_printable = is_printable(&chars[i - 1]);

        // Skip backwards while same category
        while i > 0 && is_printable(&chars[i - 1]) == cur_printable {
            i -= 1;
        }

        window_start + i
    }

    /// Push the current cursor position onto the navigation history.
    /// Called before a "big jump" (search, bookmark, page up/down, etc.)
    /// so the user can navigate back to it later.
    pub fn push_cursor_history(&mut self) {
        let pos = self.tab().cursor_position;
        self.tab_mut().push_history(pos);
    }

    /// Navigate back in cursor history
    pub fn navigate_back(&mut self) {
        if let Some(target) = self.tab_mut().history_back() {
            self.clear_selection();
            self.move_position(target);
        }
    }

    /// Navigate forward in cursor history
    pub fn navigate_forward(&mut self) {
        if let Some(target) = self.tab_mut().history_forward() {
            self.clear_selection();
            self.move_position(target);
        }
    }
}
