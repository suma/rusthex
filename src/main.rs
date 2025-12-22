//! RustHex - A modern hex editor built with gpui
//!
//! This is the main module containing the HexEditor struct and core editor functionality.
//!
//! ## Module Organization
//!
//! - `document` - Document model and file I/O
//! - `search` - Search functionality (ASCII and Hex search)
//! - `keyboard` - Keyboard event handling and shortcuts
//! - `ui` - UI types and utility functions

mod document;
mod keyboard;
mod search;
mod ui;

use document::Document;
pub use search::SearchMode;
pub use ui::{EditPane, HexNibble};
use gpui::{
    App, Application, Bounds, Context, ExternalPaths, Focusable, FocusHandle, PathPromptOptions,
    Pixels, PromptLevel, SharedString, Timer, Window, WindowBounds, WindowOptions,
    div, prelude::*, px, rgb, size, ScrollHandle,
};
use std::time::Duration;
use gpui_component::scroll::{Scrollbar, ScrollbarState, ScrollbarShow};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, AtomicUsize, Ordering};
use std::collections::HashSet;

struct HexEditor {
    document: Document,
    bytes_per_row: usize,
    scroll_handle: ScrollHandle,
    scrollbar_state: ScrollbarState,
    cursor_position: usize,
    focus_handle: FocusHandle,
    edit_pane: EditPane,
    hex_nibble: HexNibble,
    save_message: Option<String>,
    scroll_offset: Pixels, // Current scroll position in pixels
    selection_start: Option<usize>, // Selection anchor point
    is_dragging: bool, // Track if user is currently dragging
    // Search state
    search_visible: bool,
    search_query: String,
    search_mode: SearchMode,
    search_results: Vec<usize>, // List of byte positions where matches start
    current_search_index: Option<usize>, // Index into search_results
    is_searching: bool, // Flag indicating search is in progress
    search_cancel_flag: Arc<AtomicBool>, // Flag to cancel ongoing search
    shared_search_results: Arc<Mutex<Option<Vec<usize>>>>, // Shared results from background thread
    search_match_set: HashSet<usize>, // Set of all byte positions that are part of search matches (for O(1) lookup)
    search_progress: Arc<AtomicUsize>, // Current search position for progress display
    search_total: usize, // Total bytes to search (for progress display)
}

impl HexEditor {
    fn new(cx: &mut Context<Self>) -> Self {
        Self {
            document: Document::new(),
            bytes_per_row: 16,
            scroll_handle: ScrollHandle::new(),
            scrollbar_state: ScrollbarState::default(),
            cursor_position: 0,
            focus_handle: cx.focus_handle(),
            edit_pane: EditPane::Hex,
            hex_nibble: HexNibble::High,
            save_message: None,
            scroll_offset: px(0.0),
            selection_start: None,
            is_dragging: false,
            search_visible: false,
            search_query: String::new(),
            search_mode: SearchMode::Ascii,
            search_results: Vec::new(),
            current_search_index: None,
            is_searching: false,
            search_cancel_flag: Arc::new(AtomicBool::new(false)),
            shared_search_results: Arc::new(Mutex::new(None)),
            search_match_set: HashSet::new(),
            search_progress: Arc::new(AtomicUsize::new(0)),
            search_total: 0,
        }
    }

    fn load_file(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.document.load(path)?;
        self.cursor_position = 0;
        self.selection_start = None;
        Ok(())
    }

    // Selection helper methods
    fn has_selection(&self) -> bool {
        self.selection_start.is_some()
    }

    fn selection_range(&self) -> Option<(usize, usize)> {
        self.selection_start.map(|start| {
            let end = self.cursor_position;
            if start <= end {
                (start, end)
            } else {
                (end, start)
            }
        })
    }

    fn clear_selection(&mut self) {
        self.selection_start = None;
    }

    fn with_sample_data(cx: &mut Context<Self>) -> Self {
        let mut editor = Self::new(cx);
        // Sample data: Generate more data to test scrolling
        let mut data = Vec::new();

        // Add header text
        data.extend_from_slice(b"Rust Hex Editor - Sample Binary File\n");
        data.extend_from_slice(b"=====================================\n\n");

        // Add multiple lines of varied data
        for i in 0..100 {
            data.extend_from_slice(format!("Line {:03}: ", i).as_bytes());
            // Add printable characters
            for j in 0..16 {
                let byte = (0x41 + ((i + j) % 26)) as u8; // A-Z cycling
                data.push(byte);
            }
            data.push(b'\n');

            // Add some binary data
            for j in 0..16 {
                data.push(((i * 16 + j) % 256) as u8);
            }
        }

        editor.document = Document::with_data(data);
        editor
    }

    // Phase 5: Ensure cursor is visible by scrolling to its row
    fn ensure_cursor_visible_by_row(&mut self) {
        let cursor_row = self.cursor_position / self.bytes_per_row;
        self.scroll_handle.scroll_to_item(cursor_row);
    }

    // Cursor movement methods
    fn move_cursor_left(&mut self) {
        if self.cursor_position > 0 {
            self.cursor_position -= 1;
            self.hex_nibble = HexNibble::High;
            self.ensure_cursor_visible_by_row();
        }
    }

    fn move_cursor_right(&mut self) {
        if self.cursor_position < self.document.len().saturating_sub(1) {
            self.cursor_position += 1;
            self.hex_nibble = HexNibble::High;
            self.ensure_cursor_visible_by_row();
        }
    }

    fn move_cursor_up(&mut self) {
        if self.cursor_position >= self.bytes_per_row {
            self.cursor_position -= self.bytes_per_row;
            self.hex_nibble = HexNibble::High;
            self.ensure_cursor_visible_by_row();
        }
    }

    fn move_cursor_down(&mut self) {
        let new_pos = self.cursor_position + self.bytes_per_row;
        if new_pos < self.document.len() {
            self.cursor_position = new_pos;
            self.hex_nibble = HexNibble::High;
            self.ensure_cursor_visible_by_row();
        }
    }

    // Toggle between Hex and ASCII pane
    fn toggle_pane(&mut self) {
        self.edit_pane = match self.edit_pane {
            EditPane::Hex => EditPane::Ascii,
            EditPane::Ascii => EditPane::Hex,
        };
        self.hex_nibble = HexNibble::High;
    }

    // Handle hex input (0-9, A-F)
    fn input_hex(&mut self, c: char) {
        if self.cursor_position >= self.document.len() {
            return;
        }

        // Parse hex digit
        let digit = match c.to_ascii_uppercase() {
            '0'..='9' => c as u8 - b'0',
            'A'..='F' => c.to_ascii_uppercase() as u8 - b'A' + 10,
            _ => return, // Invalid hex character
        };

        let current_byte = self.document.get_byte(self.cursor_position).unwrap();

        let new_byte = match self.hex_nibble {
            HexNibble::High => {
                // Update upper 4 bits
                self.hex_nibble = HexNibble::Low;
                (digit << 4) | (current_byte & 0x0F)
            }
            HexNibble::Low => {
                // Update lower 4 bits
                self.hex_nibble = HexNibble::High;
                (current_byte & 0xF0) | digit
            }
        };

        if let Err(e) = self.document.set_byte(self.cursor_position, new_byte) {
            eprintln!("Failed to set byte: {}", e);
            return;
        }

        // Auto-advance to next byte after completing a full byte edit
        if self.hex_nibble == HexNibble::High {
            self.move_cursor_right();
        }
    }

    // Handle ASCII input
    fn input_ascii(&mut self, c: char) {
        if self.cursor_position >= self.document.len() {
            return;
        }

        // Only accept printable ASCII characters
        if c >= ' ' && c <= '~' {
            if let Err(e) = self.document.set_byte(self.cursor_position, c as u8) {
                eprintln!("Failed to set byte: {}", e);
                return;
            }
            // Auto-advance to next byte
            self.move_cursor_right();
        }
    }

    // Save file to current path (internal, no confirmation)
    fn save_file(&mut self) -> std::io::Result<()> {
        self.document.save()?;
        if let Some(path) = self.document.file_path() {
            self.save_message = Some(format!("Saved to {}", path.display()));
        }
        Ok(())
    }

    // Save file to a new path
    fn save_file_as(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.document.save_as(path.clone())?;
        self.save_message = Some(format!("Saved to {}", path.display()));
        Ok(())
    }

    /// Save file with confirmation dialog
    pub fn save_with_confirmation(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        // Check if there are unsaved changes
        if !self.document.has_unsaved_changes() {
            self.save_message = Some("No changes to save".to_string());
            cx.notify();
            return;
        }

        // Get file path for the dialog message
        let file_name = self.document.file_name()
            .unwrap_or("file")
            .to_string();

        let receiver = window.prompt(
            PromptLevel::Warning,
            "Save File",
            Some(&format!("Do you want to save changes to '{}'?", file_name)),
            &["Save", "Cancel"],
            cx,
        );

        cx.spawn_in(window, async move |entity, cx| {
            if let Ok(answer) = receiver.await {
                if answer == 0 {
                    // User clicked "Save"
                    let _ = entity.update(cx, |editor, cx| {
                        match editor.save_file() {
                            Ok(_) => {
                                eprintln!("File saved successfully");
                            }
                            Err(e) => {
                                eprintln!("Failed to save file: {}", e);
                                editor.save_message = Some(format!("Error: {}", e));
                            }
                        }
                        cx.notify();
                    });
                }
            }
        })
        .detach();
    }

    /// Open file dialog and load selected file
    pub fn open_file_dialog(&mut self, cx: &mut Context<Self>) {
        let options = PathPromptOptions {
            files: true,
            directories: false,
            multiple: false,
            prompt: Some(SharedString::from("Open File")),
        };

        let receiver = cx.prompt_for_paths(options);

        cx.spawn(async move |entity, cx| {
            if let Ok(Ok(Some(paths))) = receiver.await {
                if let Some(path) = paths.into_iter().next() {
                    let _ = entity.update(cx, |editor, cx| {
                        match editor.load_file(path.clone()) {
                            Ok(_) => {
                                editor.save_message = Some(format!("Opened: {}", path.display()));
                            }
                            Err(e) => {
                                editor.save_message = Some(format!("Error: {}", e));
                            }
                        }
                        cx.notify();
                    });
                }
            }
        })
        .detach();
    }

    /// Handle file drop event
    pub fn handle_file_drop(&mut self, paths: &ExternalPaths, cx: &mut Context<Self>) {
        if let Some(path) = paths.paths().first() {
            match self.load_file(path.clone()) {
                Ok(_) => {
                    self.save_message = Some(format!("Opened: {}", path.display()));
                }
                Err(e) => {
                    self.save_message = Some(format!("Error: {}", e));
                }
            }
            cx.notify();
        }
    }

    /// Start a background refresh loop to update UI during search
    ///
    /// This spawns an async task that periodically triggers UI updates
    /// while a search is in progress, ensuring the progress display stays current.
    pub fn start_search_refresh_loop(&mut self, cx: &mut Context<Self>) {
        cx.spawn(async move |entity, cx| {
            loop {
                // Wait 500ms between updates
                Timer::after(Duration::from_millis(500)).await;

                // Check if we should stop (search cancelled or completed)
                let should_continue = entity.update(cx, |editor, cx| {
                    if editor.is_searching {
                        cx.notify(); // Trigger UI refresh
                        true
                    } else {
                        false
                    }
                });

                match should_continue {
                    Ok(true) => continue,
                    _ => break,
                }
            }
        })
        .detach();
    }
}

impl Focusable for HexEditor {
    fn focus_handle(&self, _cx: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl Render for HexEditor {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        // Update search results if search completed
        if self.update_search_results() {
            cx.notify(); // Trigger re-render
        }

        let row_count = ui::row_count(self.document.len(), self.bytes_per_row);

        // Phase 1: Get current scroll position
        let scroll_position = self.scroll_handle.offset();
        self.scroll_offset = scroll_position.y;

        // Phase 2: Calculate visible range based on viewport
        let viewport_bounds = self.scroll_handle.bounds();
        let viewport_height = viewport_bounds.size.height;
        let (render_start, render_end) = ui::calculate_visible_range(
            self.scroll_offset,
            viewport_height,
            row_count,
            self.bytes_per_row,
        );

        // Phase 3: Calculate spacer heights for virtual scrolling
        let row_height = px(20.0);
        let top_spacer_height = row_height * render_start as f32;
        let bottom_spacer_height = row_height * (row_count - render_end) as f32;

        // Get display title
        let title = self.document.file_name()
            .unwrap_or("Rust Hex Editor")
            .to_string();

        div()
            .flex()
            .flex_col()
            .bg(rgb(0x1e1e1e))
            .size_full()
            .p_4()
            .track_focus(&self.focus_handle)
            .on_mouse_down(gpui::MouseButton::Left, cx.listener(|_this, _event, window, cx| {
                cx.focus_self(window);
            }))
            .on_mouse_up(gpui::MouseButton::Left, cx.listener(|this, _event, _window, cx| {
                if this.is_dragging {
                    this.is_dragging = false;
                    cx.notify();
                }
            }))
            .on_key_down(cx.listener(keyboard::handle_key_event))
            .on_drop(cx.listener(|editor, paths: &ExternalPaths, _window, cx| {
                editor.handle_file_drop(paths, cx);
            }))
            .child(
                // Header
                div()
                    .flex()
                    .flex_col()
                    .pb_4()
                    .border_b_1()
                    .border_color(rgb(0x404040))
                    .child(
                        div()
                            .text_xl()
                            .text_color(rgb(0xffffff))
                            .child(format!("{}{}",
                                title.clone(),
                                if self.document.has_unsaved_changes() { " *" } else { "" }
                            ))
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child(format!("{} bytes{}",
                                self.document.len(),
                                if self.document.has_unsaved_changes() { " (modified)" } else { "" }
                            ))
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child(format!("Edit Mode: {} | Tab: switch | Shift+Arrow: select | Ctrl+A: select all",
                                match self.edit_pane {
                                    EditPane::Hex => "HEX",
                                    EditPane::Ascii => "ASCII",
                                }))
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child("Ctrl+O: open | Ctrl+S: save | Ctrl+Z: undo | Ctrl+Y: redo")
                    )
                    .when(self.save_message.is_some(), |header| {
                        header.child(
                            div()
                                .text_sm()
                                .text_color(rgb(0x00ff00))
                                .child(self.save_message.as_ref().unwrap().clone())
                        )
                    })
            )
            .when(self.search_visible, |parent| {
                let search_mode_label = match self.search_mode {
                    SearchMode::Ascii => "ASCII",
                    SearchMode::Hex => "HEX",
                };
                let result_count = self.search_results.len();
                let current_pos = if let Some(idx) = self.current_search_index {
                    idx + 1
                } else {
                    0
                };

                parent.child(
                    // Search bar
                    div()
                        .flex()
                        .flex_col()
                        .py_2()
                        .px_4()
                        .bg(rgb(0x2a2a2a))
                        .border_b_1()
                        .border_color(rgb(0x404040))
                        .child(
                            div()
                                .flex()
                                .gap_4()
                                .items_center()
                                .child(
                                    div()
                                        .text_sm()
                                        .text_color(rgb(0xffffff))
                                        .child(format!("Search ({}): {}", search_mode_label, self.search_query))
                                )
                                .when(self.is_searching, |d| {
                                    d.child(
                                        div()
                                            .text_sm()
                                            .text_color(rgb(0xffff00))
                                            .child("Searching...")
                                    )
                                })
                                .when(!self.is_searching && result_count > 0, |d| {
                                    d.child(
                                        div()
                                            .text_sm()
                                            .text_color(rgb(0x00ff00))
                                            .child(format!("{} / {} matches", current_pos, result_count))
                                    )
                                })
                                .when(!self.is_searching && result_count == 0 && !self.search_query.is_empty(), |d| {
                                    d.child(
                                        div()
                                            .text_sm()
                                            .text_color(rgb(0xff0000))
                                            .child("No matches")
                                    )
                                })
                        )
                        .child(
                            div()
                                .text_xs()
                                .text_color(rgb(0x808080))
                                .child("Type to search | Tab: switch mode | Enter/F3: next | Shift+F3: prev | Backspace: delete | Esc: close")
                        )
                )
            })
            .child(
                // Content area with scrollbar
                div()
                    .flex()
                    .flex_1()
                    .pt_4()
                    .relative()
                    .overflow_hidden()
                    .child(
                        // Scrollable content
                        div()
                            .id("hex-content")
                            .absolute()
                            .top_0()
                            .left_0()
                            .right_0()
                            .bottom_0()
                            .overflow_y_scroll()
                            .track_scroll(&self.scroll_handle)
                            .pr(px(24.0))
                            .child(
                                div()
                                    .flex()
                                    .flex_col()
                                    .font_family("Monaco")
                                    .text_sm()
                                    // Phase 3: Top spacer for virtual scrolling
                                    .when(render_start > 0, |parent| {
                                        parent.child(
                                            div().h(top_spacer_height)
                                        )
                                    })
                                    // Render only visible rows
                                    .children((render_start..render_end).map(|row| {
                        let address = ui::format_address(row * self.bytes_per_row);
                        let start = row * self.bytes_per_row;
                        let end = (start + self.bytes_per_row).min(self.document.len());
                        let cursor_pos = self.cursor_position;
                        let edit_pane = self.edit_pane;
                        let selection_range = self.selection_range();

                        div()
                            .flex()
                            .gap_4()
                            .mb_1()
                            .child(
                                // Address column
                                div()
                                    .w(px(80.0))
                                    .text_color(rgb(0x808080))
                                    .child(address)
                            )
                            .child(
                                // Hex column - each byte separately
                                div()
                                    .flex()
                                    .gap_1()
                                    .flex_1()
                                    .children((start..end).map(|byte_idx| {
                                        let byte = self.document.get_byte(byte_idx).unwrap();
                                        let hex_str = format!("{:02X}", byte);
                                        let is_cursor = byte_idx == cursor_pos;
                                        let is_selected = selection_range
                                            .map(|(sel_start, sel_end)| byte_idx >= sel_start && byte_idx <= sel_end)
                                            .unwrap_or(false);

                                        // Check if this byte is part of a search result (O(1) lookup)
                                        let is_search_match = self.search_match_set.contains(&byte_idx);

                                        // Check if this byte is part of the current (highlighted) search result
                                        let is_current_search = if let Some(idx) = self.current_search_index {
                                            let match_start = self.search_results[idx];
                                            let pattern_len = match self.search_mode {
                                                SearchMode::Ascii => self.search_query.len(),
                                                SearchMode::Hex => self.search_query.split_whitespace().count(),
                                            };
                                            byte_idx >= match_start && byte_idx < match_start + pattern_len
                                        } else {
                                            false
                                        };

                                        div()
                                            .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event, _window, cx| {
                                                this.cursor_position = byte_idx;
                                                this.selection_start = Some(byte_idx);
                                                this.is_dragging = true;
                                                this.edit_pane = EditPane::Hex;
                                                this.hex_nibble = HexNibble::High;
                                                cx.notify();
                                            }))
                                            .on_mouse_move(cx.listener(move |this, _event, _window, cx| {
                                                if this.is_dragging {
                                                    this.cursor_position = byte_idx;
                                                    cx.notify();
                                                }
                                            }))
                                            .when(is_cursor && edit_pane == EditPane::Hex, |div| {
                                                div.bg(rgb(0x4a9eff))
                                                    .text_color(rgb(0x000000))
                                            })
                                            .when(is_cursor && edit_pane == EditPane::Ascii, |div| {
                                                div.bg(rgb(0x333333))
                                                    .text_color(rgb(0x00ff00))
                                            })
                                            .when(!is_cursor && is_current_search, |div| {
                                                div.bg(rgb(0xff8c00))
                                                    .text_color(rgb(0x000000))
                                            })
                                            .when(!is_cursor && !is_current_search && is_search_match, |div| {
                                                div.bg(rgb(0xffff00))
                                                    .text_color(rgb(0x000000))
                                            })
                                            .when(!is_cursor && !is_current_search && !is_search_match && is_selected, |div| {
                                                div.bg(rgb(0x505050))
                                                    .text_color(rgb(0xffffff))
                                            })
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected, |div| {
                                                div.text_color(rgb(0x00ff00))
                                            })
                                            .child(hex_str)
                                    }))
                            )
                            .child(
                                // ASCII column - each byte separately
                                div()
                                    .flex()
                                    .w(px(160.0))
                                    .children((start..end).map(|byte_idx| {
                                        let byte = self.document.get_byte(byte_idx).unwrap();
                                        let ascii_char = if byte >= 0x20 && byte <= 0x7E {
                                            byte as char
                                        } else {
                                            '.'
                                        };
                                        let is_cursor = byte_idx == cursor_pos;
                                        let is_selected = selection_range
                                            .map(|(sel_start, sel_end)| byte_idx >= sel_start && byte_idx <= sel_end)
                                            .unwrap_or(false);

                                        // Check if this byte is part of a search result (O(1) lookup)
                                        let is_search_match = self.search_match_set.contains(&byte_idx);

                                        // Check if this byte is part of the current (highlighted) search result
                                        let is_current_search = if let Some(idx) = self.current_search_index {
                                            let match_start = self.search_results[idx];
                                            let pattern_len = match self.search_mode {
                                                SearchMode::Ascii => self.search_query.len(),
                                                SearchMode::Hex => self.search_query.split_whitespace().count(),
                                            };
                                            byte_idx >= match_start && byte_idx < match_start + pattern_len
                                        } else {
                                            false
                                        };

                                        div()
                                            .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event, _window, cx| {
                                                this.cursor_position = byte_idx;
                                                this.selection_start = Some(byte_idx);
                                                this.is_dragging = true;
                                                this.edit_pane = EditPane::Ascii;
                                                this.hex_nibble = HexNibble::High;
                                                cx.notify();
                                            }))
                                            .on_mouse_move(cx.listener(move |this, _event, _window, cx| {
                                                if this.is_dragging {
                                                    this.cursor_position = byte_idx;
                                                    cx.notify();
                                                }
                                            }))
                                            .when(is_cursor && edit_pane == EditPane::Ascii, |div| {
                                                div.bg(rgb(0xff8c00))
                                                    .text_color(rgb(0x000000))
                                            })
                                            .when(is_cursor && edit_pane == EditPane::Hex, |div| {
                                                div.bg(rgb(0x333333))
                                                    .text_color(rgb(0xffffff))
                                            })
                                            .when(!is_cursor && is_current_search, |div| {
                                                div.bg(rgb(0xff8c00))
                                                    .text_color(rgb(0x000000))
                                            })
                                            .when(!is_cursor && !is_current_search && is_search_match, |div| {
                                                div.bg(rgb(0xffff00))
                                                    .text_color(rgb(0x000000))
                                            })
                                            .when(!is_cursor && !is_current_search && !is_search_match && is_selected, |div| {
                                                div.bg(rgb(0x505050))
                                                    .text_color(rgb(0xffffff))
                                            })
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected, |div| {
                                                div.text_color(rgb(0xffffff))
                                            })
                                            .child(ascii_char.to_string())
                                    }))
                            )
                    }))
                                    // Phase 3: Bottom spacer for virtual scrolling
                                    .when(render_end < row_count, |parent| {
                                        parent.child(
                                            div().h(bottom_spacer_height)
                                        )
                                    })
                            )
                    )
                    .child(
                        // Scrollbar with visible background and search markers
                        div()
                            .absolute()
                            .top_0()
                            .right_0()
                            .bottom_0()
                            .w(px(12.0))
                            .bg(rgb(0x2a2a2a))
                            .child(
                                Scrollbar::vertical(&self.scrollbar_state, &self.scroll_handle)
                                    .scrollbar_show(ScrollbarShow::Always)
                            )
                            // Add search result markers on scrollbar
                            .children({
                                let total_rows = ui::row_count(self.document.len(), self.bytes_per_row);
                                let viewport_height = viewport_bounds.size.height;

                                if total_rows == 0 || viewport_height <= px(0.0) {
                                    Vec::new()
                                } else {
                                    self.search_results.iter().map(|&result_pos| {
                                        let result_row = result_pos / self.bytes_per_row;
                                        let position_ratio = result_row as f32 / total_rows as f32;

                                        // Calculate position in pixels relative to viewport height
                                        // Use viewport height as approximation for scrollbar height
                                        let marker_position = viewport_height * position_ratio;

                                        // Check if this is the current search result
                                        let is_current = self.current_search_index
                                            .map(|idx| self.search_results[idx] == result_pos)
                                            .unwrap_or(false);

                                        div()
                                            .absolute()
                                            .left(px(0.0))
                                            .w(px(12.0))
                                            .h(px(3.0))
                                            .top(marker_position)
                                            .bg(if is_current {
                                                rgb(0xff8c00) // Orange for current result
                                            } else {
                                                rgb(0xffff00) // Yellow for other results
                                            })
                                            .opacity(0.8)
                                    }).collect::<Vec<_>>()
                                }
                            })
                    )
            )
            .child(
                // Status bar at bottom (two lines)
                div()
                    .flex()
                    .flex_col()
                    .py_1()
                    .px_4()
                    .bg(rgb(0x252525))
                    .border_t_1()
                    .border_color(rgb(0x404040))
                    .text_sm()
                    .font_family("Monaco")
                    // First line: cursor position, byte value, selection
                    .child(
                        div()
                            .flex()
                            .gap_4()
                            .py_1()
                            .child(
                                // Cursor position
                                div()
                                    .flex()
                                    .gap_2()
                                    .text_color(rgb(0x808080))
                                    .child("Offset:")
                                    .child(
                                        div()
                                            .text_color(rgb(0x00ff00))
                                            .child(ui::format_address(self.cursor_position))
                                    )
                            )
                            .child(
                                // Current byte value (if valid position)
                                div()
                                    .flex()
                                    .gap_2()
                                    .text_color(rgb(0x808080))
                                    .when_some(self.document.get_byte(self.cursor_position), |el, byte| {
                                        el.child("Value:")
                                            .child(
                                                div()
                                                    .text_color(rgb(0x4a9eff))
                                                    .child(ui::format_byte_hex(byte))
                                            )
                                            .child(
                                                div()
                                                    .text_color(rgb(0xffffff))
                                                    .child(format!("({})", ui::format_byte_dec(byte)))
                                            )
                                            .child(
                                                div()
                                                    .text_color(rgb(0xff8c00))
                                                    .child(ui::format_byte_bin(byte))
                                            )
                                    })
                            )
                            .when(self.selection_start.is_some(), |el| {
                                // Selection info
                                let (start, end) = self.selection_range().unwrap();
                                let selection_size = end - start + 1;
                                el.child(
                                    div()
                                        .flex()
                                        .gap_2()
                                        .text_color(rgb(0x808080))
                                        .child("Selection:")
                                        .child(
                                            div()
                                                .text_color(rgb(0xffff00))
                                                .child(format!("{} bytes", selection_size))
                                        )
                                )
                            })
                            .child(
                                // File size (right aligned)
                                div()
                                    .flex_1()
                                    .text_right()
                                    .text_color(rgb(0x808080))
                                    .child(format!("Size: {}", ui::format_file_size(self.document.len())))
                            )
                    )
                    // Second line: search status, messages
                    .child(
                        div()
                            .flex()
                            .gap_4()
                            .py_1()
                            .border_t_1()
                            .border_color(rgb(0x333333))
                            // Search status with progress
                            .when(self.is_searching, |el| {
                                let current = self.search_progress.load(Ordering::Relaxed);
                                let total = self.search_total;
                                let percent = if total > 0 {
                                    (current as f64 / total as f64 * 100.0) as usize
                                } else {
                                    0
                                };
                                el.child(
                                    div()
                                        .flex()
                                        .gap_2()
                                        .text_color(rgb(0xffff00))
                                        .child("Searching...")
                                        .child(
                                            div()
                                                .text_color(rgb(0x808080))
                                                .child(format!("\"{}\"", self.search_query))
                                        )
                                        .child(
                                            div()
                                                .text_color(rgb(0x4a9eff))
                                                .child(format!("{}/{} ({}%)",
                                                    ui::format_file_size(current),
                                                    ui::format_file_size(total),
                                                    percent
                                                ))
                                        )
                                )
                            })
                            // Search results info (when not searching)
                            .when(!self.is_searching && self.search_visible && !self.search_query.is_empty(), |el| {
                                let result_count = self.search_results.len();
                                let current_pos = self.current_search_index.map(|i| i + 1).unwrap_or(0);
                                let mode_str = match self.search_mode {
                                    SearchMode::Ascii => "ASCII",
                                    SearchMode::Hex => "Hex",
                                };
                                el.child(
                                    div()
                                        .flex()
                                        .gap_2()
                                        .child(
                                            div()
                                                .text_color(rgb(0x808080))
                                                .child(format!("Search ({}):", mode_str))
                                        )
                                        .child(
                                            div()
                                                .text_color(rgb(0x4a9eff))
                                                .child(format!("\"{}\"", self.search_query))
                                        )
                                        .child(
                                            div()
                                                .text_color(if result_count > 0 { rgb(0x00ff00) } else { rgb(0xff6666) })
                                                .child(if result_count > 0 {
                                                    format!("{}/{} matches", current_pos, result_count)
                                                } else {
                                                    "No matches".to_string()
                                                })
                                        )
                                )
                            })
                            // Save/status messages
                            .when_some(self.save_message.clone(), |el, msg| {
                                el.child(
                                    div()
                                        .text_color(rgb(0x00ff00))
                                        .child(msg)
                                )
                            })
                            // Default message when nothing else to show
                            .when(!self.is_searching && !self.search_visible && self.save_message.is_none(), |el| {
                                el.child(
                                    div()
                                        .text_color(rgb(0x606060))
                                        .child("Ready")
                                )
                            })
                    )
            )
    }
}

fn main() {
    // Get command line arguments
    let args: Vec<String> = std::env::args().collect();

    Application::new().run(move |cx: &mut App| {
        // Initialize gpui-component theme for Scrollbar support
        gpui_component::theme::init(cx);

        let bounds = Bounds::centered(None, size(px(800.0), px(600.0)), cx);
        cx.open_window(
            WindowOptions {
                window_bounds: Some(WindowBounds::Windowed(bounds)),
                ..Default::default()
            },
            move |window, cx| {
                cx.new(move |cx| {
                    // If file path is provided, load it; otherwise use sample data
                    let editor = if args.len() > 1 {
                        let file_path = PathBuf::from(&args[1]);
                        let mut editor = HexEditor::new(cx);
                        match editor.load_file(file_path.clone()) {
                            Ok(_) => editor,
                            Err(e) => {
                                eprintln!("Failed to load file: {}", e);
                                HexEditor::with_sample_data(cx)
                            }
                        }
                    } else {
                        HexEditor::with_sample_data(cx)
                    };

                    // Set initial focus
                    cx.focus_self(window);
                    editor
                })
            },
        )
        .unwrap();
        cx.activate(true);
    });
}
