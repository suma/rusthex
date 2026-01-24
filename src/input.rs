//! Input handling functionality for the hex editor
//!
//! This module contains methods for handling byte input:
//! - Hex input (0-9, A-F)
//! - ASCII input
//! - Pane toggling and selection management

use crate::ui::{EditPane, HexNibble};
use crate::HexEditor;

impl HexEditor {
    /// Handle hex input (0-9, A-F)
    pub fn input_hex(&mut self, c: char) {
        if self.tab().cursor_position >= self.tab().document.len() {
            return;
        }

        // Parse hex digit
        let digit = match c.to_ascii_uppercase() {
            '0'..='9' => c as u8 - b'0',
            'A'..='F' => c.to_ascii_uppercase() as u8 - b'A' + 10,
            _ => return, // Invalid hex character
        };

        let cursor_pos = self.tab().cursor_position;
        let current_byte = self.tab().document.get_byte(cursor_pos).unwrap();

        let new_byte = match self.tab().hex_nibble {
            HexNibble::High => {
                // Update upper 4 bits
                self.tab_mut().hex_nibble = HexNibble::Low;
                (digit << 4) | (current_byte & 0x0F)
            }
            HexNibble::Low => {
                // Update lower 4 bits
                self.tab_mut().hex_nibble = HexNibble::High;
                (current_byte & 0xF0) | digit
            }
        };

        let cursor_pos = self.tab().cursor_position;
        if let Err(e) = self.tab_mut().document.set_byte(cursor_pos, new_byte) {
            eprintln!("Failed to set byte: {}", e);
            return;
        }

        // Auto-advance to next byte after completing a full byte edit
        if self.tab().hex_nibble == HexNibble::High {
            self.move_cursor_right();
        }
    }

    /// Handle ASCII input
    pub fn input_ascii(&mut self, c: char) {
        if self.tab().cursor_position >= self.tab().document.len() {
            return;
        }

        // Only accept printable ASCII characters
        if c >= ' ' && c <= '~' {
            let cursor_pos = self.tab().cursor_position;
            if let Err(e) = self.tab_mut().document.set_byte(cursor_pos, c as u8) {
                eprintln!("Failed to set byte: {}", e);
                return;
            }
            // Auto-advance to next byte
            self.move_cursor_right();
        }
    }

    /// Toggle between Hex and ASCII pane
    pub fn toggle_pane(&mut self) {
        let new_pane = match self.tab().edit_pane {
            EditPane::Hex => EditPane::Ascii,
            EditPane::Ascii => EditPane::Hex,
        };
        self.tab_mut().edit_pane = new_pane;
        self.tab_mut().hex_nibble = HexNibble::High;
    }

    // Selection helper methods

    /// Check if there is an active selection
    pub fn has_selection(&self) -> bool {
        self.tab().selection_start.is_some()
    }

    /// Get the selection range as (start, end) tuple
    pub fn selection_range(&self) -> Option<(usize, usize)> {
        self.tab().selection_start.map(|start| {
            let end = self.tab().cursor_position;
            if start <= end {
                (start, end)
            } else {
                (end, start)
            }
        })
    }

    /// Clear the current selection
    pub fn clear_selection(&mut self) {
        self.tab_mut().selection_start = None;
    }
}
