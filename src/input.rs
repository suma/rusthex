//! Input handling functionality for the hex editor
//!
//! This module contains methods for handling byte input:
//! - Hex input (0-9, A-F)
//! - ASCII input
//! - Pane toggling and selection management

use gpui::{ClipboardItem, Context, PromptLevel, Window};

use crate::HexEditor;
use crate::ui::{EditMode, EditPane, HexNibble};

impl HexEditor {
    /// Handle hex input (0-9, A-F)
    pub fn input_hex(&mut self, c: char) {
        // Parse hex digit
        let digit = match c.to_ascii_uppercase() {
            '0'..='9' => c as u8 - b'0',
            'A'..='F' => c.to_ascii_uppercase() as u8 - b'A' + 10,
            _ => return, // Invalid hex character
        };

        let cursor_pos = self.tab().cursor_position;

        if self.tab().edit_mode == EditMode::Insert {
            // Insert mode
            match self.tab().hex_nibble {
                HexNibble::High => {
                    // High nibble: insert a new byte
                    let byte = digit << 4;
                    if let Err(e) = self.tab_mut().document.insert_bytes(cursor_pos, &[byte]) {
                        eprintln!("Failed to insert byte: {}", e);
                        return;
                    }
                    self.invalidate_render_cache();
                    self.tab_mut().hex_nibble = HexNibble::Low;
                }
                HexNibble::Low => {
                    // Low nibble: update the lower nibble of the inserted byte
                    if let Some(current_byte) = self.tab().document.get_byte(cursor_pos) {
                        let new_byte = (current_byte & 0xF0) | digit;
                        if let Err(e) = self.tab_mut().document.set_byte(cursor_pos, new_byte) {
                            eprintln!("Failed to set byte: {}", e);
                            return;
                        }
                        self.tab_mut().hex_nibble = HexNibble::High;
                        self.move_cursor_right();
                    }
                }
            }
        } else {
            // Overwrite mode
            if cursor_pos >= self.tab().document.len() {
                return;
            }

            let current_byte = self.tab().document.get_byte(cursor_pos).unwrap();

            let new_byte = match self.tab().hex_nibble {
                HexNibble::High => {
                    self.tab_mut().hex_nibble = HexNibble::Low;
                    (digit << 4) | (current_byte & 0x0F)
                }
                HexNibble::Low => {
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
    }

    /// Handle ASCII input
    pub fn input_ascii(&mut self, c: char) {
        // Only accept printable ASCII characters
        if c < ' ' || c > '~' {
            return;
        }

        let cursor_pos = self.tab().cursor_position;

        if self.tab().edit_mode == EditMode::Insert {
            // Insert mode: insert byte
            if let Err(e) = self.tab_mut().document.insert_bytes(cursor_pos, &[c as u8]) {
                eprintln!("Failed to insert byte: {}", e);
                return;
            }
            self.invalidate_render_cache();
            self.move_cursor_right();
        } else {
            // Overwrite mode
            if cursor_pos >= self.tab().document.len() {
                return;
            }
            if let Err(e) = self.tab_mut().document.set_byte(cursor_pos, c as u8) {
                eprintln!("Failed to set byte: {}", e);
                return;
            }
            self.move_cursor_right();
        }
    }

    /// Toggle between Hex and ASCII pane
    pub fn toggle_pane(&mut self) {
        let new_pane = match self.tab().edit_pane {
            EditPane::Hex => EditPane::Ascii,
            EditPane::Ascii => EditPane::Hex,
            EditPane::Bitmap => EditPane::Hex,
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

    /// Get selected bytes with start offset. Returns None if no valid selection.
    pub(crate) fn get_selected_bytes(&self) -> Option<(usize, Vec<u8>)> {
        let (start, end) = if let Some(range) = self.selection_range() {
            range
        } else {
            let pos = self.tab().cursor_position;
            if pos >= self.tab().document.len() {
                return None;
            }
            (pos, pos)
        };

        let bytes = self.tab().document.get_slice(start..end + 1)?;
        if bytes.is_empty() {
            None
        } else {
            Some((start, bytes))
        }
    }

    /// Smart copy: ASCII text when in ASCII pane, hex string when in Hex pane
    pub fn copy_selection(&mut self, cx: &mut Context<Self>) {
        match self.tab().edit_pane {
            EditPane::Ascii => self.copy_as_ascii(cx),
            _ => self.copy_as_hex_string(cx),
        }
    }

    /// Copy selected bytes as space-separated hex string (e.g. "4A 6F 68 6E")
    pub fn copy_as_hex_string(&mut self, cx: &mut Context<Self>) {
        if let Some((_start, bytes)) = self.get_selected_bytes() {
            let hex_string: String = bytes
                .iter()
                .map(|b| format!("{:02X}", b))
                .collect::<Vec<_>>()
                .join(" ");
            cx.write_to_clipboard(ClipboardItem::new_string(hex_string));
            self.log(crate::log_panel::LogLevel::Info, format!("Copied {} byte(s) as hex", bytes.len()));
        }
    }

    /// Copy selected bytes as ASCII text (non-printable chars replaced with '.')
    pub fn copy_as_ascii(&mut self, cx: &mut Context<Self>) {
        if let Some((_start, bytes)) = self.get_selected_bytes() {
            let ascii: String = bytes
                .iter()
                .map(|&b| if (0x20..=0x7E).contains(&b) { b as char } else { '.' })
                .collect();
            cx.write_to_clipboard(ClipboardItem::new_string(ascii));
            self.log(crate::log_panel::LogLevel::Info, format!("Copied {} byte(s) as ASCII", bytes.len()));
        }
    }

    /// Copy selected bytes as C array (e.g. "{ 0x4A, 0x6F, 0x68, 0x6E }")
    pub fn copy_as_c_array(&mut self, cx: &mut Context<Self>) {
        if let Some((_start, bytes)) = self.get_selected_bytes() {
            let items: String = bytes
                .iter()
                .map(|b| format!("0x{:02X}", b))
                .collect::<Vec<_>>()
                .join(", ");
            let c_array = format!("{{ {} }}", items);
            cx.write_to_clipboard(ClipboardItem::new_string(c_array));
            self.log(crate::log_panel::LogLevel::Info, format!("Copied {} byte(s) as C array", bytes.len()));
        }
    }

    /// Paste hex string from clipboard
    pub fn paste_from_clipboard(&mut self, cx: &mut Context<Self>) {
        let clipboard = cx.read_from_clipboard();
        let text = match clipboard.as_ref().and_then(|item| item.text()) {
            Some(t) => t.to_string(),
            None => return,
        };

        // Parse hex bytes from clipboard text (e.g. "4A 6F 68 6E" or "4A6F686E")
        let bytes: Vec<u8> = text
            .split(|c: char| c.is_whitespace() || c == ',')
            .filter(|s| !s.is_empty())
            .filter_map(|s| u8::from_str_radix(s.trim(), 16).ok())
            .collect();

        if bytes.is_empty() {
            return;
        }

        let cursor_pos = self.tab().cursor_position;
        if self.tab().edit_mode == EditMode::Insert {
            if let Err(e) = self.tab_mut().document.insert_bytes(cursor_pos, &bytes) {
                eprintln!("Failed to paste bytes: {}", e);
                return;
            }
            self.invalidate_render_cache();
        } else {
            // Overwrite mode: overwrite bytes starting at cursor
            for (i, &byte) in bytes.iter().enumerate() {
                let pos = cursor_pos + i;
                if pos >= self.tab().document.len() {
                    break;
                }
                if let Err(e) = self.tab_mut().document.set_byte(pos, byte) {
                    eprintln!("Failed to paste byte: {}", e);
                    break;
                }
            }
        }
        self.log(crate::log_panel::LogLevel::Info, format!("Pasted {} byte(s)", bytes.len()));
    }

    /// Open selected bytes in a new tab
    pub fn open_selection_in_new_tab(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        let (start, end) = match self.selection_range() {
            Some(range) => range,
            None => {
                self.log(crate::log_panel::LogLevel::Info, "No selection to open");
                return;
            }
        };

        let byte_count = end - start + 1;
        let base_name = self
            .tab()
            .document
            .file_name()
            .unwrap_or("untitled")
            .to_string();
        let name = format!("{} [0x{:X}-0x{:X}]", base_name, start, end);

        const LARGE_SELECTION_THRESHOLD: usize = 100 * 1024 * 1024; // 100 MiB

        if byte_count >= LARGE_SELECTION_THRESHOLD {
            let size_mib = byte_count as f64 / (1024.0 * 1024.0);
            let receiver = window.prompt(
                PromptLevel::Warning,
                "Large Selection",
                Some(&format!(
                    "The selection is {:.1} MiB. Opening it in a new tab will use additional memory.\n\nDo you want to continue?",
                    size_mib
                )),
                &["Open", "Cancel"],
                cx,
            );

            cx.spawn_in(window, async move |entity, cx| {
                if let Ok(answer) = receiver.await {
                    if answer == 0 {
                        let _ = entity.update(cx, |editor, cx| {
                            let bytes = match editor.tab().document.get_slice(start..end + 1) {
                                Some(b) if !b.is_empty() => b,
                                _ => return,
                            };
                            editor.open_data_in_new_tab(bytes, name);
                            editor.log(
                                crate::log_panel::LogLevel::Info,
                                format!("Opened {} byte(s) in new tab", byte_count),
                            );
                            cx.notify();
                        });
                    }
                }
            })
            .detach();
        } else {
            let bytes = match self.tab().document.get_slice(start..end + 1) {
                Some(b) if !b.is_empty() => b,
                _ => {
                    self.log(crate::log_panel::LogLevel::Info, "No data in selection");
                    return;
                }
            };
            self.open_data_in_new_tab(bytes, name);
            self.log(
                crate::log_panel::LogLevel::Info,
                format!("Opened {} byte(s) in new tab", byte_count),
            );
        }
    }
}
