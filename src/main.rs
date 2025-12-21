mod document;

use document::Document;
use gpui::{
    App, Application, Bounds, Context, Focusable, FocusHandle, KeyDownEvent, Pixels, Window, WindowBounds, WindowOptions,
    div, prelude::*, px, rgb, size, ScrollHandle,
};
use gpui_component::scroll::{Scrollbar, ScrollbarState, ScrollbarShow};
use std::path::PathBuf;

#[derive(Debug, Clone, Copy, PartialEq)]
enum EditPane {
    Hex,
    Ascii,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum HexNibble {
    High,  // Upper nibble (first hex digit)
    Low,   // Lower nibble (second hex digit)
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum SearchMode {
    Ascii,
    Hex,
}

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

    // Search methods
    fn perform_search(&mut self) {
        self.search_results.clear();
        self.current_search_index = None;

        if self.search_query.is_empty() {
            return;
        }

        let pattern = match self.search_mode {
            SearchMode::Ascii => {
                // ASCII search: convert query string to bytes
                self.search_query.as_bytes().to_vec()
            }
            SearchMode::Hex => {
                // Hex search: parse space-separated hex values
                let hex_parts: Vec<&str> = self.search_query.split_whitespace().collect();
                let mut pattern = Vec::new();
                for part in hex_parts {
                    if let Ok(byte) = u8::from_str_radix(part, 16) {
                        pattern.push(byte);
                    } else {
                        // Invalid hex, abort search
                        return;
                    }
                }
                pattern
            }
        };

        if pattern.is_empty() {
            return;
        }

        // Search for pattern in document
        let data_len = self.document.len();
        let pattern_len = pattern.len();

        for i in 0..=data_len.saturating_sub(pattern_len) {
            let mut matches = true;
            for j in 0..pattern_len {
                if let Some(byte) = self.document.get_byte(i + j) {
                    if byte != pattern[j] {
                        matches = false;
                        break;
                    }
                } else {
                    matches = false;
                    break;
                }
            }
            if matches {
                self.search_results.push(i);
            }
        }

        // Set current index to first result
        if !self.search_results.is_empty() {
            self.current_search_index = Some(0);
            self.cursor_position = self.search_results[0];
        }
    }

    fn next_search_result(&mut self) {
        if self.search_results.is_empty() {
            return;
        }

        if let Some(current_idx) = self.current_search_index {
            let next_idx = (current_idx + 1) % self.search_results.len();
            self.current_search_index = Some(next_idx);
            self.cursor_position = self.search_results[next_idx];
            self.ensure_cursor_visible_by_row();
        }
    }

    fn prev_search_result(&mut self) {
        if self.search_results.is_empty() {
            return;
        }

        if let Some(current_idx) = self.current_search_index {
            let prev_idx = if current_idx == 0 {
                self.search_results.len() - 1
            } else {
                current_idx - 1
            };
            self.current_search_index = Some(prev_idx);
            self.cursor_position = self.search_results[prev_idx];
            self.ensure_cursor_visible_by_row();
        }
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

    fn format_address(&self, offset: usize) -> String {
        format!("{:08X}", offset)
    }

    fn row_count(&self) -> usize {
        (self.document.len() + self.bytes_per_row - 1) / self.bytes_per_row
    }

    // Calculate visible row range for virtual scrolling
    fn calculate_visible_range(&self, viewport_height: Pixels) -> (usize, usize) {
        let row_height = px(20.0);
        let total_rows = self.row_count();

        // Calculate first visible row
        // Note: scroll_offset is negative when scrolling down in gpui
        let scroll_offset_abs = self.scroll_offset * -1.0;
        let first_visible_row = if scroll_offset_abs > px(0.0) {
            (scroll_offset_abs / row_height).floor() as usize
        } else {
            0
        };

        // Calculate number of visible rows
        let visible_row_count = (viewport_height / row_height).ceil() as usize;

        // Add buffer rows to prevent flickering during scroll
        let buffer_rows = 5;
        let mut render_start = first_visible_row.saturating_sub(buffer_rows);
        let render_end = (first_visible_row + visible_row_count + buffer_rows).min(total_rows);

        // Ensure we render at least visible_row_count rows when near the end
        if render_end == total_rows {
            // At the end of the file, adjust render_start to show full viewport
            let desired_render_count = visible_row_count + buffer_rows * 2;
            render_start = total_rows.saturating_sub(desired_render_count);
        }

        (render_start, render_end)
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

    // Save file to current path
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
}

impl Focusable for HexEditor {
    fn focus_handle(&self, _cx: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl Render for HexEditor {
    fn render(&mut self, _window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        let row_count = self.row_count();

        // Phase 1: Get current scroll position
        let scroll_position = self.scroll_handle.offset();
        self.scroll_offset = scroll_position.y;

        // Phase 2: Calculate visible range based on viewport
        let viewport_bounds = self.scroll_handle.bounds();
        let viewport_height = viewport_bounds.size.height;
        let (render_start, render_end) = self.calculate_visible_range(viewport_height);

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
            .on_key_down(cx.listener(|this: &mut Self, event: &KeyDownEvent, _window: &mut Window, cx: &mut Context<Self>| {
                // Check for Ctrl+F or Cmd+F (toggle search)
                if event.keystroke.key == "f" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform) {
                    this.search_visible = !this.search_visible;
                    if !this.search_visible {
                        this.search_results.clear();
                        this.current_search_index = None;
                    }
                    cx.notify();
                    return;
                }

                // Check for Escape (close search)
                if event.keystroke.key == "escape" && this.search_visible {
                    this.search_visible = false;
                    this.search_results.clear();
                    this.current_search_index = None;
                    cx.notify();
                    return;
                }

                // Check for F3 (next search result)
                if event.keystroke.key == "f3" && !event.keystroke.modifiers.shift {
                    this.next_search_result();
                    cx.notify();
                    return;
                }

                // Check for Shift+F3 (previous search result)
                if event.keystroke.key == "f3" && event.keystroke.modifiers.shift {
                    this.prev_search_result();
                    cx.notify();
                    return;
                }

                // Check for Ctrl+A or Cmd+A (select all)
                if event.keystroke.key == "a" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform) && !event.keystroke.modifiers.shift {
                    if this.document.len() > 0 {
                        this.selection_start = Some(0);
                        this.cursor_position = this.document.len().saturating_sub(1);
                        this.save_message = Some("Selected all".to_string());
                        cx.notify();
                    }
                    return;
                }

                // Check for Ctrl+S or Cmd+S (save)
                if event.keystroke.key == "s" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform) {
                    match this.save_file() {
                        Ok(_) => {
                            eprintln!("File saved successfully");
                            cx.notify();
                        }
                        Err(e) => {
                            eprintln!("Failed to save file: {}", e);
                            this.save_message = Some(format!("Error: {}", e));
                            cx.notify();
                        }
                    }
                    return;
                }

                // Check for Ctrl+Z or Cmd+Z (undo)
                if event.keystroke.key == "z" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform) && !event.keystroke.modifiers.shift {
                    if this.document.undo() {
                        this.save_message = Some("Undo".to_string());
                        cx.notify();
                    }
                    return;
                }

                // Check for Ctrl+Y or Cmd+Y or Ctrl+Shift+Z (redo)
                if (event.keystroke.key == "y" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)) ||
                   (event.keystroke.key == "z" && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform) && event.keystroke.modifiers.shift) {
                    if this.document.redo() {
                        this.save_message = Some("Redo".to_string());
                        cx.notify();
                    }
                    return;
                }

                match event.keystroke.key.as_str() {
                    "up" => {
                        if event.keystroke.modifiers.shift {
                            // Start selection if not already selecting
                            if this.selection_start.is_none() {
                                this.selection_start = Some(this.cursor_position);
                            }
                            this.move_cursor_up();
                        } else {
                            this.clear_selection();
                            this.move_cursor_up();
                        }
                        cx.notify();
                    }
                    "down" => {
                        if event.keystroke.modifiers.shift {
                            if this.selection_start.is_none() {
                                this.selection_start = Some(this.cursor_position);
                            }
                            this.move_cursor_down();
                        } else {
                            this.clear_selection();
                            this.move_cursor_down();
                        }
                        cx.notify();
                    }
                    "left" => {
                        if event.keystroke.modifiers.shift {
                            if this.selection_start.is_none() {
                                this.selection_start = Some(this.cursor_position);
                            }
                            this.move_cursor_left();
                        } else {
                            this.clear_selection();
                            this.move_cursor_left();
                        }
                        cx.notify();
                    }
                    "right" => {
                        if event.keystroke.modifiers.shift {
                            if this.selection_start.is_none() {
                                this.selection_start = Some(this.cursor_position);
                            }
                            this.move_cursor_right();
                        } else {
                            this.clear_selection();
                            this.move_cursor_right();
                        }
                        cx.notify();
                    }
                    "tab" => {
                        if this.search_visible {
                            // Toggle search mode
                            this.search_mode = match this.search_mode {
                                SearchMode::Ascii => SearchMode::Hex,
                                SearchMode::Hex => SearchMode::Ascii,
                            };
                            this.perform_search();
                        } else {
                            this.toggle_pane();
                        }
                        cx.notify();
                    }
                    "backspace" => {
                        if this.search_visible {
                            this.search_query.pop();
                            this.perform_search();
                            cx.notify();
                        }
                    }
                    "enter" => {
                        if this.search_visible {
                            this.next_search_result();
                            cx.notify();
                        }
                    }
                    key => {
                        // Handle character input
                        if key.len() == 1 {
                            let c = key.chars().next().unwrap();

                            if this.search_visible {
                                // Add character to search query
                                if (c >= ' ' && c <= '~') || c.is_whitespace() {
                                    this.search_query.push(c);
                                    this.perform_search();
                                    cx.notify();
                                }
                            } else {
                                // Normal editing
                                match this.edit_pane {
                                    EditPane::Hex => {
                                        if c.is_ascii_hexdigit() {
                                            this.input_hex(c);
                                            cx.notify();
                                        }
                                    }
                                    EditPane::Ascii => {
                                        if c >= ' ' && c <= '~' {
                                            this.input_ascii(c);
                                            cx.notify();
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
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
                            .child("Ctrl+S: save | Ctrl+Z: undo | Ctrl+Y: redo")
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
                                .when(result_count > 0, |d| {
                                    d.child(
                                        div()
                                            .text_sm()
                                            .text_color(rgb(0x00ff00))
                                            .child(format!("{} / {} matches", current_pos, result_count))
                                    )
                                })
                                .when(result_count == 0 && !self.search_query.is_empty(), |d| {
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
                        let address = self.format_address(row * self.bytes_per_row);
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

                                        // Check if this byte is part of a search result
                                        let is_search_match = self.search_results.iter().any(|&match_start| {
                                            let pattern_len = match self.search_mode {
                                                SearchMode::Ascii => self.search_query.len(),
                                                SearchMode::Hex => self.search_query.split_whitespace().count(),
                                            };
                                            byte_idx >= match_start && byte_idx < match_start + pattern_len
                                        });
                                        let is_current_search = self.current_search_index.map(|idx| {
                                            let match_start = self.search_results[idx];
                                            let pattern_len = match self.search_mode {
                                                SearchMode::Ascii => self.search_query.len(),
                                                SearchMode::Hex => self.search_query.split_whitespace().count(),
                                            };
                                            byte_idx >= match_start && byte_idx < match_start + pattern_len
                                        }).unwrap_or(false);

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

                                        // Check if this byte is part of a search result
                                        let is_search_match = self.search_results.iter().any(|&match_start| {
                                            let pattern_len = match self.search_mode {
                                                SearchMode::Ascii => self.search_query.len(),
                                                SearchMode::Hex => self.search_query.split_whitespace().count(),
                                            };
                                            byte_idx >= match_start && byte_idx < match_start + pattern_len
                                        });
                                        let is_current_search = self.current_search_index.map(|idx| {
                                            let match_start = self.search_results[idx];
                                            let pattern_len = match self.search_mode {
                                                SearchMode::Ascii => self.search_query.len(),
                                                SearchMode::Hex => self.search_query.split_whitespace().count(),
                                            };
                                            byte_idx >= match_start && byte_idx < match_start + pattern_len
                                        }).unwrap_or(false);

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
                        // Scrollbar with visible background
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
