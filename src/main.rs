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

mod compare;
mod config;
mod document;
mod encoding;
mod keyboard;
mod render_cache;
mod search;
mod tab;
mod tabs;
mod ui;

use config::Settings;
use document::Document;
use render_cache::{CacheState, RenderCache};
pub use search::SearchMode;
use tab::EditorTab;
pub use ui::{EditPane, HexNibble, Endian, TextEncoding};
use encoding::{decode_for_display, DisplayChar};
use gpui::{
    App, Application, Bounds, Context, ExternalPaths, Focusable, FocusHandle, Font, FontFeatures,
    FontStyle, FontWeight, PathPromptOptions, Point, PromptLevel, SharedString, Timer, Window,
    WindowBounds, WindowOptions, div, prelude::*, px, rgb, rgba, size,
};
use std::time::Duration;
use std::sync::atomic::Ordering;
use gpui_component::scroll::{Scrollbar, ScrollbarShow};
use std::path::PathBuf;

struct HexEditor {
    tabs: Vec<EditorTab>,
    active_tab: usize,
    settings: Settings,
    focus_handle: FocusHandle,
    save_message: Option<String>,
    content_view_rows: usize,
    is_dragging: bool,
    drag_pane: Option<EditPane>,
    // Data inspector state (global)
    inspector_visible: bool,
    inspector_endian: Endian,
    // Tab drag state
    dragging_tab_index: Option<usize>,
    tab_drop_target: Option<usize>,
    // Compare mode state
    compare_mode: bool,
    compare_tab_index: Option<usize>,
    compare_selection_visible: bool,
    // Cached row height (calculated from font metrics in render)
    cached_row_height: f32,
    // Cached line heights for different text sizes (calculated from font metrics)
    cached_line_height_xl: f32,
    cached_line_height_sm: f32,
    // Text encoding for ASCII column display
    text_encoding: TextEncoding,
    // Cached monospace character width (calculated from font metrics in render)
    cached_char_width: f32,
}

impl HexEditor {
    fn new(cx: &mut Context<Self>) -> Self {
        let settings = Settings::load();
        let inspector_endian = match settings.editor.default_endian {
            config::DefaultEndian::Little => Endian::Little,
            config::DefaultEndian::Big => Endian::Big,
        };
        Self {
            tabs: vec![EditorTab::new()],
            active_tab: 0,
            settings,
            focus_handle: cx.focus_handle(),
            save_message: None,
            content_view_rows: 20,
            is_dragging: false,
            drag_pane: None,
            inspector_visible: false,
            inspector_endian,
            dragging_tab_index: None,
            tab_drop_target: None,
            compare_mode: false,
            compare_tab_index: None,
            compare_selection_visible: false,
            cached_row_height: 24.0, // Default, will be updated in render()
            cached_line_height_xl: 24.0, // Default, will be updated in render()
            cached_line_height_sm: 17.0, // Default, will be updated in render()
            text_encoding: TextEncoding::default(),
            cached_char_width: 8.4, // Default (14 * 0.6), will be updated in render()
        }
    }

    /// Get bytes per row from settings
    fn bytes_per_row(&self) -> usize {
        self.settings.display.bytes_per_row
    }

    /// Get row height (cached from font metrics calculation in render())
    fn row_height(&self) -> f64 {
        self.cached_row_height as f64
    }

    /// Calculate header height based on cached font metrics
    /// Header content + Tab bar (conditional) + Search bar (conditional)
    fn calculate_header_height(&self) -> f32 {
        // Header div: text_xl + 3 * text_sm + pb_4(16) + border_b_1(1)
        // Using minimal estimates for line heights
        let base_header = self.cached_line_height_xl + 3.0 * self.cached_line_height_sm + 17.0;

        // Tab bar (conditional): ~30px total
        let tab_bar = if self.tabs.len() > 1 {
            self.cached_line_height_sm + 12.0
        } else {
            0.0
        };

        // Search bar (conditional): ~35px total
        let search_bar = if self.tab().search_visible {
            self.cached_line_height_sm + 20.0
        } else {
            0.0
        };

        base_header + tab_bar + search_bar
    }

    /// Calculate status bar height based on cached font metrics
    fn calculate_status_bar_height(&self) -> f32 {
        // Two lines of text_sm with minimal padding: ~45px total
        2.0 * self.cached_line_height_sm + 20.0
    }

    /// Calculate total non-content height (header + status bar)
    fn calculate_non_content_height(&self) -> f32 {
        let calculated = self.calculate_header_height() + self.calculate_status_bar_height();
        // Debug: print calculated value
        // eprintln!("non_content_height: {}, header: {}, status: {}", calculated, self.calculate_header_height(), self.calculate_status_bar_height());
        calculated
    }

    /// Get current active tab
    fn tab(&self) -> &EditorTab {
        &self.tabs[self.active_tab]
    }

    /// Get current active tab mutably
    fn tab_mut(&mut self) -> &mut EditorTab {
        &mut self.tabs[self.active_tab]
    }

    /// Toggle data inspector visibility
    fn toggle_inspector(&mut self) {
        self.inspector_visible = !self.inspector_visible;
    }

    /// Toggle inspector endianness
    fn toggle_inspector_endian(&mut self) {
        self.inspector_endian = match self.inspector_endian {
            Endian::Little => Endian::Big,
            Endian::Big => Endian::Little,
        };
    }

    /// Cycle to next text encoding
    fn cycle_encoding(&mut self) {
        self.text_encoding = self.text_encoding.next();
        self.invalidate_render_cache();
    }

    /// Set text encoding directly
    fn set_encoding(&mut self, encoding: TextEncoding) {
        if self.text_encoding != encoding {
            self.text_encoding = encoding;
            self.invalidate_render_cache();
        }
    }

    fn load_file(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.tab_mut().document.load(path)?;
        self.tab_mut().cursor_position = 0;
        self.tab_mut().selection_start = None;
        Ok(())
    }

    // Selection helper methods
    fn has_selection(&self) -> bool {
        self.tab().selection_start.is_some()
    }

    fn selection_range(&self) -> Option<(usize, usize)> {
        self.tab().selection_start.map(|start| {
            let end = self.tab().cursor_position;
            if start <= end {
                (start, end)
            } else {
                (end, start)
            }
        })
    }

    fn clear_selection(&mut self) {
        self.tab_mut().selection_start = None;
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

        editor.tab_mut().document = Document::with_data(data);
        editor
    }

    // Ensure cursor is visible by scrolling to its row
    fn ensure_cursor_visible_by_row(&mut self) {
        let cursor_row = self.tab().cursor_position / self.bytes_per_row();
        let total_rows = ui::row_count(self.tab().document.len(), self.bytes_per_row());
        let current_offset = self.tab().scroll_handle.offset();

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

    // Cursor movement methods
    fn move_cursor_left(&mut self) {
        if self.tab().cursor_position > 0 {
            self.tab_mut().cursor_position -= 1;
            self.tab_mut().hex_nibble = HexNibble::High;
            self.ensure_cursor_visible_by_row();
        }
    }

    fn move_cursor_right(&mut self) {
        if self.tab().cursor_position < self.tab().document.len().saturating_sub(1) {
            self.tab_mut().cursor_position += 1;
            self.tab_mut().hex_nibble = HexNibble::High;
            self.ensure_cursor_visible_by_row();
        }
    }

    fn move_cursor_up(&mut self) {
        if self.tab().cursor_position >= self.bytes_per_row() {
            self.tab_mut().cursor_position -= self.bytes_per_row();
            self.tab_mut().hex_nibble = HexNibble::High;
            self.ensure_cursor_visible_by_row();
        }
    }

    fn move_cursor_down(&mut self) {
        let new_pos = self.tab().cursor_position + self.bytes_per_row();
        if new_pos < self.tab().document.len() {
            self.tab_mut().cursor_position = new_pos;
            self.tab_mut().hex_nibble = HexNibble::High;
            self.ensure_cursor_visible_by_row();
        }
    }

    // Page Up: Move cursor up by one page
    fn move_cursor_page_up(&mut self, visible_rows: usize) {
        let rows_to_move = visible_rows.saturating_sub(1).max(1);
        let bytes_to_move = rows_to_move * self.bytes_per_row();
        let new_pos = self.tab().cursor_position.saturating_sub(bytes_to_move);
        self.tab_mut().cursor_position = new_pos;
        self.tab_mut().hex_nibble = HexNibble::High;
        self.ensure_cursor_visible_by_row();
    }

    // Page Down: Move cursor down by one page
    fn move_cursor_page_down(&mut self, visible_rows: usize) {
        let rows_to_move = visible_rows.saturating_sub(1).max(1);
        let bytes_to_move = rows_to_move * self.bytes_per_row();
        let new_pos = self.tab().cursor_position + bytes_to_move;
        if new_pos < self.tab().document.len() {
            self.tab_mut().cursor_position = new_pos;
        } else {
            self.tab_mut().cursor_position = self.tab().document.len().saturating_sub(1);
        }
        self.tab_mut().hex_nibble = HexNibble::High;
        self.ensure_cursor_visible_by_row();
    }

    // Home: Move cursor to the beginning of the current row
    fn move_cursor_home(&mut self) {
        let current_row = self.tab().cursor_position / self.bytes_per_row();
        self.tab_mut().cursor_position = current_row * self.bytes_per_row();
        self.tab_mut().hex_nibble = HexNibble::High;
        self.ensure_cursor_visible_by_row();
    }

    // End: Move cursor to the end of the current row
    fn move_cursor_end(&mut self) {
        let current_row = self.tab().cursor_position / self.bytes_per_row();
        let row_end = ((current_row + 1) * self.bytes_per_row()).min(self.tab().document.len()) - 1;
        self.tab_mut().cursor_position = row_end;
        self.tab_mut().hex_nibble = HexNibble::High;
        self.ensure_cursor_visible_by_row();
    }

    // Ctrl+Home: Move cursor to the beginning of the file
    fn move_cursor_file_start(&mut self) {
        self.tab_mut().cursor_position = 0;
        self.tab_mut().hex_nibble = HexNibble::High;
        self.ensure_cursor_visible_by_row();
    }

    // Ctrl+End: Move cursor to the end of the file
    fn move_cursor_file_end(&mut self) {
        if self.tab().document.len() > 0 {
            self.tab_mut().cursor_position = self.tab().document.len() - 1;
        }
        self.tab_mut().hex_nibble = HexNibble::High;
        self.ensure_cursor_visible_by_row();
    }

    // Toggle between Hex and ASCII pane
    fn toggle_pane(&mut self) {
        let new_pane = match self.tab().edit_pane {
            EditPane::Hex => EditPane::Ascii,
            EditPane::Ascii => EditPane::Hex,
        };
        self.tab_mut().edit_pane = new_pane;
        self.tab_mut().hex_nibble = HexNibble::High;
    }

    // Handle hex input (0-9, A-F)
    fn input_hex(&mut self, c: char) {
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

    // Handle ASCII input
    fn input_ascii(&mut self, c: char) {
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

    // Save file to current path (internal, no confirmation)
    fn save_file(&mut self) -> std::io::Result<()> {
        self.tab_mut().document.save()?;
        if let Some(path) = self.tab().document.file_path() {
            self.save_message = Some(format!("Saved to {}", path.display()));
        }
        Ok(())
    }

    // Save file to a new path
    fn save_file_as(&mut self, path: PathBuf) -> std::io::Result<()> {
        self.tab_mut().document.save_as(path.clone())?;
        self.save_message = Some(format!("Saved to {}", path.display()));
        Ok(())
    }

    /// Save file with confirmation dialog
    pub fn save_with_confirmation(&mut self, window: &mut Window, cx: &mut Context<Self>) {
        // Check if there are unsaved changes
        if !self.tab().document.has_unsaved_changes() {
            self.save_message = Some("No changes to save".to_string());
            cx.notify();
            return;
        }

        // Get file path for the dialog message
        let file_name = self.tab().document.file_name()
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
                        match editor.open_file_in_new_tab(path.clone()) {
                            Ok(_) => {
                                editor.save_message = Some(format!("Opened in new tab: {}", path.display()));
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

    /// Save As dialog - save file to a new location
    pub fn save_as_dialog(&mut self, cx: &mut Context<Self>) {
        // Get directory and suggested filename from current document
        let (directory, suggested_name) = if let Some(path) = self.tab().document.file_path() {
            let dir = path.parent().map(|p| p.to_path_buf()).unwrap_or_else(|| PathBuf::from("."));
            let name = path.file_name().map(|n| n.to_string_lossy().to_string());
            (dir, name)
        } else {
            (PathBuf::from("."), Some("untitled.bin".to_string()))
        };

        let receiver = cx.prompt_for_new_path(&directory, suggested_name.as_deref());

        cx.spawn(async move |entity, cx| {
            if let Ok(Ok(Some(path))) = receiver.await {
                let _ = entity.update(cx, |editor, cx| {
                    match editor.save_file_as(path.clone()) {
                        Ok(_) => {
                            editor.save_message = Some(format!("Saved as: {}", path.display()));
                        }
                        Err(e) => {
                            editor.save_message = Some(format!("Error: {}", e));
                        }
                    }
                    cx.notify();
                });
            }
        })
        .detach();
    }

    /// Handle file drop event - open in new tab
    pub fn handle_file_drop(&mut self, paths: &ExternalPaths, cx: &mut Context<Self>) {
        if let Some(path) = paths.paths().first() {
            match self.open_file_in_new_tab(path.clone()) {
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
                    if editor.tab().is_searching {
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

    /// Build current cache state for comparison
    fn build_cache_state(&self) -> CacheState {
        let selection_range = self.selection_range();
        CacheState {
            cursor_position: self.tab().cursor_position,
            selection_start: selection_range.map(|(s, _)| s),
            selection_end: selection_range.map(|(_, e)| e),
            search_results_count: self.tab().search_results.len(),
            current_search_index: self.tab().current_search_index,
            document_len: self.tab().document.len(),
            bytes_per_row: self.bytes_per_row(),
            text_encoding: self.text_encoding,
        }
    }

    /// Get current search range for highlighting
    fn current_search_range(&self) -> Option<(usize, usize)> {
        self.tab().current_search_index.map(|idx| {
            let match_start = self.tab().search_results[idx];
            let pattern_len = match self.tab().search_mode {
                SearchMode::Ascii => self.tab().search_query.len(),
                SearchMode::Hex => self.tab().search_query.split_whitespace().count(),
            };
            (match_start, match_start + pattern_len)
        })
    }

    /// Update render cache for visible rows
    fn update_render_cache(&mut self, render_start: usize, render_end: usize) {
        let cache_state = self.build_cache_state();

        // Check if full cache rebuild is needed
        let full_rebuild = !self.tab().render_cache.is_valid(&cache_state);

        let bytes_per_row = self.bytes_per_row();
        let document_len = self.tab().document.len();
        let cursor_position = self.tab().cursor_position;
        let selection_range = self.selection_range();
        let current_search_range = self.current_search_range();

        for row in render_start..render_end {
            // Skip if row is cached and doesn't need rebuild
            if !full_rebuild
                && self.tab().render_cache.get_row(row).is_some()
                && !self.tab().render_cache.row_needs_rebuild(row, bytes_per_row)
            {
                continue;
            }

            // Build row data
            let row_data = RenderCache::build_row_data(
                row,
                bytes_per_row,
                document_len,
                cursor_position,
                selection_range,
                &self.tab().search_match_set,
                current_search_range,
                |idx| self.tab().document.get_byte(idx),
            );

            self.tab_mut().render_cache.cache_row(row, row_data);
        }

        // Update cache state
        self.tab_mut().render_cache.set_state(cache_state);
    }

    /// Invalidate render cache (call when document content changes)
    pub fn invalidate_render_cache(&mut self) {
        self.tab_mut().render_cache.invalidate();
    }

    /// Mark a byte offset as modified in the render cache
    pub fn mark_byte_modified(&mut self, offset: usize) {
        self.tab_mut().render_cache.mark_modified(offset);
    }
}

impl Focusable for HexEditor {
    fn focus_handle(&self, _cx: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

impl Render for HexEditor {
    fn render(&mut self, window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        // Calculate and cache font metrics for different text sizes
        let font = Font {
            family: "Monaco".into(),
            features: FontFeatures::default(),
            fallbacks: None,
            weight: FontWeight::default(),
            style: FontStyle::Normal,
        };
        let font_id = window.text_system().resolve_font(&font);

        // Main content font (configurable size)
        let font_size = px(self.settings.display.font_size);
        let ascent = window.text_system().ascent(font_id, font_size);
        let descent = window.text_system().descent(font_id, font_size);
        // Row height = line height + mb_1 margin (4px)
        self.cached_row_height = f32::from(ascent) + f32::from(descent).abs() + 4.0;
        // Monospace character width from em_advance (fallback to ascent * 0.6 if unavailable)
        self.cached_char_width = match window.text_system().em_advance(font_id, font_size) {
            Ok(advance) => f32::from(advance),
            Err(_) => f32::from(ascent) * 0.6, // Fallback approximation
        };

        // text_xl (20px) for header title
        let xl_size = px(20.0);
        let xl_ascent = window.text_system().ascent(font_id, xl_size);
        let xl_descent = window.text_system().descent(font_id, xl_size);
        self.cached_line_height_xl = f32::from(xl_ascent) + f32::from(xl_descent).abs();

        // text_sm (14px) for status bar and info lines
        let sm_size = px(14.0);
        let sm_ascent = window.text_system().ascent(font_id, sm_size);
        let sm_descent = window.text_system().descent(font_id, sm_size);
        self.cached_line_height_sm = f32::from(sm_ascent) + f32::from(sm_descent).abs();

        // Update search results if search completed
        if self.update_search_results() {
            cx.notify(); // Trigger re-render
        }

        let row_count = ui::row_count(self.tab().document.len(), self.bytes_per_row());

        // Phase 1: Get current scroll position
        let scroll_position = self.tab().scroll_handle.offset();
        self.tab_mut().scroll_offset = scroll_position.y;

        // Phase 2: Calculate visible range based on viewport
        let viewport_bounds = self.tab().scroll_handle.bounds();
        let viewport_height = viewport_bounds.size.height;

        // Calculate actual content area height by subtracting header and status bar
        // Uses dynamically calculated heights based on font metrics
        let non_content_height = self.calculate_non_content_height();
        let content_height = px((f32::from(viewport_height) - non_content_height).max(20.0));

        let visible_range = ui::calculate_visible_range(
            self.tab().scroll_offset,
            content_height,
            row_count,
            self.row_height(),
        );
        let render_start = visible_range.render_start;
        let render_end = visible_range.render_end;

        // Update content_view_rows from calculated visible rows
        self.content_view_rows = visible_range.visible_rows;

        // Update render cache for visible rows
        self.update_render_cache(render_start, render_end);

        // Phase 3: Calculate spacer heights for virtual scrolling
        // Uses capped virtual height to avoid f32 precision issues with large files
        let (top_spacer_height, bottom_spacer_height) = ui::calculate_spacer_heights(
            render_start,
            render_end,
            row_count,
            self.row_height(),
        );

        // Get display title
        let title = self.tab().document.file_name()
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
                    this.drag_pane = None;
                    cx.notify();
                }
            }))
            .on_mouse_move(cx.listener(|this, event: &gpui::MouseMoveEvent, _window, cx| {
                // Cancel drag if mouse button was released outside the window
                if this.is_dragging && !event.dragging() {
                    this.is_dragging = false;
                    this.drag_pane = None;
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
                                if self.tab().document.has_unsaved_changes() { " *" } else { "" }
                            ))
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child(format!("{} bytes{}",
                                self.tab().document.len(),
                                if self.tab().document.has_unsaved_changes() { " (modified)" } else { "" }
                            ))
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child(format!("Edit Mode: {} | Tab: switch | Shift+Arrow: select | Ctrl+A: select all",
                                match self.tab().edit_pane {
                                    EditPane::Hex => "HEX",
                                    EditPane::Ascii => "ASCII",
                                }))
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(rgb(0x808080))
                            .child("Ctrl+O: open | Ctrl+S: save | Ctrl+T: new tab | Ctrl+W: close | Ctrl+K: compare | Ctrl+Z/Y: undo/redo")
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
            // Tab bar
            .when(self.tabs.len() > 1, |parent| {
                let dragging_tab = self.dragging_tab_index;
                let drop_target = self.tab_drop_target;
                parent.child(
                    div()
                        .flex()
                        .gap_1()
                        .py_1()
                        .px_2()
                        .bg(rgb(0x252525))
                        .border_b_1()
                        .border_color(rgb(0x404040))
                        .on_mouse_up(gpui::MouseButton::Left, cx.listener(|this, _event, _window, cx| {
                            // Complete the drag operation
                            if let (Some(from), Some(to)) = (this.dragging_tab_index, this.tab_drop_target) {
                                if from != to {
                                    this.reorder_tab(from, to);
                                }
                            }
                            this.dragging_tab_index = None;
                            this.tab_drop_target = None;
                            cx.notify();
                        }))
                        .children(
                            self.tabs.iter().enumerate().map(|(idx, tab)| {
                                let is_active = idx == self.active_tab;
                                let tab_name = tab.display_name();
                                let is_being_dragged = dragging_tab == Some(idx);
                                let is_drop_target = drop_target == Some(idx) && dragging_tab != Some(idx);
                                div()
                                    .id(("tab", idx))
                                    .flex()
                                    .items_center()
                                    .gap_2()
                                    .px_3()
                                    .py_1()
                                    .text_sm()
                                    .cursor_pointer()
                                    .rounded_t_md()
                                    // Drop target indicator (left border)
                                    .when(is_drop_target, |d| {
                                        d.border_l_2()
                                            .border_color(rgb(0x4a9eff))
                                    })
                                    // Dragging state (semi-transparent)
                                    .when(is_being_dragged, |d| {
                                        d.opacity(0.5)
                                    })
                                    .when(is_active && !is_being_dragged, |d| {
                                        d.bg(rgb(0x1e1e1e))
                                            .text_color(rgb(0xffffff))
                                            .border_t_1()
                                            .border_l_1()
                                            .border_r_1()
                                            .border_color(rgb(0x404040))
                                    })
                                    .when(!is_active && !is_being_dragged, |d| {
                                        d.bg(rgb(0x2a2a2a))
                                            .text_color(rgb(0x808080))
                                            .hover(|h| h.bg(rgb(0x333333)))
                                    })
                                    .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event, _window, cx| {
                                        this.dragging_tab_index = Some(idx);
                                        this.switch_to_tab(idx);
                                        cx.notify();
                                    }))
                                    .on_mouse_move(cx.listener(move |this, event: &gpui::MouseMoveEvent, _window, cx| {
                                        // Update drop target when dragging over tabs
                                        if this.dragging_tab_index.is_some() && event.dragging() {
                                            if this.tab_drop_target != Some(idx) {
                                                this.tab_drop_target = Some(idx);
                                                cx.notify();
                                            }
                                        }
                                    }))
                                    .child(tab_name)
                                    .child(
                                        // Close button
                                        div()
                                            .id(("tab-close", idx))
                                            .text_xs()
                                            .text_color(rgb(0x808080))
                                            .hover(|h| h.text_color(rgb(0xff6666)))
                                            .cursor_pointer()
                                            .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event: &gpui::MouseDownEvent, _window, cx| {
                                                // Close this specific tab
                                                if this.tabs.len() > 1 {
                                                    this.tabs.remove(idx);
                                                    if this.active_tab >= this.tabs.len() {
                                                        this.active_tab = this.tabs.len() - 1;
                                                    } else if this.active_tab > idx {
                                                        this.active_tab -= 1;
                                                    }
                                                }
                                                // Clear drag state
                                                this.dragging_tab_index = None;
                                                this.tab_drop_target = None;
                                                cx.notify();
                                            }))
                                            .child("×")
                                    )
                            }).collect::<Vec<_>>()
                        )
                        .child(
                            // New tab button
                            div()
                                .id("new-tab")
                                .px_2()
                                .py_1()
                                .text_sm()
                                .text_color(rgb(0x808080))
                                .cursor_pointer()
                                .hover(|h| h.text_color(rgb(0x00ff00)))
                                .on_mouse_down(gpui::MouseButton::Left, cx.listener(|this, _event, _window, cx| {
                                    this.new_tab();
                                    cx.notify();
                                }))
                                .child("+")
                        )
                )
            })
            .when(self.tab().search_visible, |parent| {
                let search_mode_label = match self.tab().search_mode {
                    SearchMode::Ascii => "ASCII",
                    SearchMode::Hex => "HEX",
                };
                let result_count = self.tab().search_results.len();
                let current_pos = if let Some(idx) = self.tab().current_search_index {
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
                                        .child(format!("Search ({}): {}", search_mode_label, self.tab().search_query))
                                )
                                .when(self.tab().is_searching, |d| {
                                    d.child(
                                        div()
                                            .text_sm()
                                            .text_color(rgb(0xffff00))
                                            .child("Searching...")
                                    )
                                })
                                .when(!self.tab().is_searching && result_count > 0, |d| {
                                    d.child(
                                        div()
                                            .text_sm()
                                            .text_color(if self.tab().search_truncated { rgb(0xffaa00) } else { rgb(0x00ff00) })
                                            .child(if self.tab().search_truncated {
                                                format!("{} / {}+ matches (truncated)", current_pos, result_count)
                                            } else {
                                                format!("{} / {} matches", current_pos, result_count)
                                            })
                                    )
                                })
                                .when(!self.tab().is_searching && result_count == 0 && !self.tab().search_query.is_empty(), |d| {
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
            // Normal mode: single pane view
            .when(!self.compare_mode, |parent| parent.child(
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
                            .track_scroll(&self.tab().scroll_handle)
                            .pr(px(24.0))
                            .on_mouse_move(cx.listener(|this, event: &gpui::MouseMoveEvent, _window, cx| {
                                // Cancel drag if mouse button was released outside the window
                                if this.is_dragging && !event.dragging() {
                                    this.is_dragging = false;
                                    this.drag_pane = None;
                                    cx.notify();
                                    return;
                                }

                                if !this.is_dragging || this.drag_pane.is_none() {
                                    return;
                                }

                                // Get current values from this (not captured snapshots)
                                let bytes_per_row = this.bytes_per_row();
                                let doc_len = this.tab().document.len();
                                let scroll_offset_f32: f32 = (-f32::from(this.tab().scroll_offset)).max(0.0);

                                // Use cached values calculated from font metrics in render()
                                let row_height = this.cached_row_height;
                                let char_width = this.cached_char_width;
                                // Hex byte: 2 chars + gap_1 (4px)
                                let hex_byte_width = char_width * 2.0 + 4.0;

                                // Calculate row from Y position
                                // event.position is in window coordinates
                                let mouse_y: f32 = event.position.y.into();

                                // Content area offset: outer padding + header height + content top padding + first row offset
                                let outer_padding = 16.0; // p_4
                                let content_top_padding = 16.0; // pt_4
                                let content_top = outer_padding + this.calculate_header_height() + content_top_padding + row_height;
                                let relative_y = mouse_y - content_top + scroll_offset_f32;

                                let row = if relative_y < 0.0 {
                                    0
                                } else {
                                    (relative_y / row_height) as usize
                                };

                                let row_start = row * bytes_per_row;

                                // Calculate byte in row from X position
                                // Window coordinates: outer padding (16) + Address (80) + gap_4 (16) = 112px for hex start
                                let mouse_x: f32 = event.position.x.into();
                                let hex_start = 112.0; // 16 + 80 + 16
                                let gap = 16.0; // gap_4
                                let gap_1 = 4.0; // gap_1 between hex bytes
                                let byte_in_row = match this.drag_pane {
                                    Some(EditPane::Hex) => {
                                        if mouse_x < hex_start {
                                            0
                                        } else {
                                            ((mouse_x - hex_start) / hex_byte_width) as usize
                                        }
                                    }
                                    Some(EditPane::Ascii) => {
                                        // Hex column: each byte is 2 chars, with gap_1 between bytes (not after last)
                                        let hex_column_width = bytes_per_row as f32 * char_width * 2.0
                                            + (bytes_per_row - 1) as f32 * gap_1;
                                        let ascii_start = hex_start + hex_column_width + gap;
                                        if mouse_x < ascii_start {
                                            0
                                        } else {
                                            // ASCII column: each character is char_width, no gaps
                                            ((mouse_x - ascii_start) / char_width) as usize
                                        }
                                    }
                                    None => 0,
                                };

                                let byte_in_row = byte_in_row.min(bytes_per_row - 1);
                                let new_cursor = (row_start + byte_in_row).min(doc_len.saturating_sub(1));

                                if this.tab().cursor_position != new_cursor {
                                    this.tab_mut().cursor_position = new_cursor;
                                    this.ensure_cursor_visible_by_row();
                                    cx.notify();
                                }
                            }))
                            .child(
                                div()
                                    .flex()
                                    .flex_col()
                                    .font_family("Monaco")
                                    .text_size(px(self.settings.display.font_size))
                                    // Phase 3: Top spacer for virtual scrolling
                                    .when(render_start > 0, |parent| {
                                        parent.child(
                                            div().h(top_spacer_height)
                                        )
                                    })
                                    // Render only visible rows using cached data
                                    .children((render_start..render_end).map(|row| {
                        // Get cached row data (always available after update_render_cache)
                        let row_data = self.tab().render_cache.get_row(row).cloned();
                        let edit_pane = self.tab().edit_pane;
                        let bytes_per_row = self.bytes_per_row();
                        let row_start = row * bytes_per_row;
                        let document_len = self.tab().document.len();

                        // Use cached data if available, otherwise compute inline
                        let (address, is_cursor_row) = match &row_data {
                            Some(data) => (data.address.clone(), data.is_cursor_row),
                            None => {
                                let addr = ui::format_address(row_start);
                                let cursor_row = self.tab().cursor_position / bytes_per_row;
                                (addr, row == cursor_row)
                            }
                        };

                        // Calculate end for this row
                        let row_end = (row_start + bytes_per_row).min(document_len);

                        // Decode bytes for ASCII column display based on current encoding
                        let row_bytes: Vec<u8> = (row_start..row_end)
                            .filter_map(|i| self.tab().document.get_byte(i))
                            .collect();
                        let decoded_chars = decode_for_display(&row_bytes, self.text_encoding);

                        div()
                            .id(("hex-row", row))  // Stable ID for efficient diffing
                            .flex()
                            .gap_4()
                            .mb_1()
                            .child(
                                // Address column - highlight cursor row
                                div()
                                    .w(px(80.0))
                                    .text_color(if is_cursor_row { rgb(0x4a9eff) } else { rgb(0x808080) })
                                    .child(address)
                            )
                            .child(
                                // Hex column - always render bytes_per_row slots for consistent width
                                div()
                                    .flex()
                                    .gap_1()
                                    .children((0..bytes_per_row).map(|i| {
                                        let byte_idx = row_start + i;
                                        let has_data = byte_idx < document_len;

                                        // Get byte data from cache or compute
                                        let (hex_str, is_cursor, is_selected, is_search_match, is_current_search) =
                                            if has_data {
                                                match &row_data {
                                                    Some(data) if i < data.bytes.len() => {
                                                        let b = &data.bytes[i];
                                                        (format!("{:02X}", b.value), b.is_cursor, b.is_selected, b.is_search_match, b.is_current_search)
                                                    }
                                                    _ => {
                                                        // Fallback: compute inline (shouldn't happen if cache is properly updated)
                                                        let byte = self.tab().document.get_byte(byte_idx).unwrap_or(0);
                                                        (format!("{:02X}", byte), false, false, false, false)
                                                    }
                                                }
                                            } else {
                                                // Empty slot - use spaces to maintain width
                                                ("  ".to_string(), false, false, false, false)
                                            };

                                        div()
                                            .id(("hex-byte", row_start * 100 + i))  // Unique ID even for empty slots
                                            .when(has_data, |div| {
                                                div.on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event: &gpui::MouseDownEvent, _window, cx| {
                                                    this.tab_mut().cursor_position = byte_idx;
                                                    this.tab_mut().selection_start = Some(byte_idx);
                                                    this.is_dragging = true;
                                                    this.drag_pane = Some(EditPane::Hex);
                                                    this.tab_mut().edit_pane = EditPane::Hex;
                                                    this.tab_mut().hex_nibble = HexNibble::High;
                                                    this.ensure_cursor_visible_by_row();
                                                    cx.notify();
                                                }))
                                            })
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
                                // ASCII column - each byte separately (using cached data + encoding)
                                div()
                                    .flex()
                                    .w(px(160.0))
                                    .children((row_start..row_end).enumerate().map(|(i, byte_idx)| {
                                        // Get display character from decoded data
                                        let display_char = decoded_chars.get(i).cloned().unwrap_or(DisplayChar::Invalid);
                                        let ascii_char = display_char.to_char();
                                        let is_continuation = display_char.is_continuation();

                                        // Get cursor/selection state from cache
                                        let (is_cursor, is_selected, is_search_match, is_current_search) =
                                            match &row_data {
                                                Some(data) if i < data.bytes.len() => {
                                                    let b = &data.bytes[i];
                                                    (b.is_cursor, b.is_selected, b.is_search_match, b.is_current_search)
                                                }
                                                _ => (false, false, false, false)
                                            };

                                        div()
                                            .id(("ascii-byte", byte_idx))  // Stable ID for efficient diffing
                                            .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event: &gpui::MouseDownEvent, _window, cx| {
                                                this.tab_mut().cursor_position = byte_idx;
                                                this.tab_mut().selection_start = Some(byte_idx);
                                                this.is_dragging = true;
                                                this.drag_pane = Some(EditPane::Ascii);
                                                this.tab_mut().edit_pane = EditPane::Ascii;
                                                this.tab_mut().hex_nibble = HexNibble::High;
                                                this.ensure_cursor_visible_by_row();
                                                cx.notify();
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
                                            // Continuation bytes shown in dim color
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected && is_continuation, |div| {
                                                div.text_color(rgb(0x606060))
                                            })
                                            // Normal characters in white
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected && !is_continuation, |div| {
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
                                Scrollbar::vertical(&self.tab().scrollbar_state, &self.tab().scroll_handle)
                                    .scrollbar_show(ScrollbarShow::Always)
                            )
                            // Add search result markers on scrollbar
                            .children({
                                let total_rows = ui::row_count(self.tab().document.len(), self.bytes_per_row());
                                let viewport_height = viewport_bounds.size.height;

                                if total_rows == 0 || viewport_height <= px(0.0) {
                                    Vec::new()
                                } else {
                                    self.tab().search_results.iter().map(|&result_pos| {
                                        let result_row = result_pos / self.bytes_per_row();
                                        let position_ratio = result_row as f32 / total_rows as f32;

                                        // Calculate position in pixels relative to viewport height
                                        // Use viewport height as approximation for scrollbar height
                                        let marker_position = viewport_height * position_ratio;

                                        // Check if this is the current search result
                                        let is_current = self.tab().current_search_index
                                            .map(|idx| self.tab().search_results[idx] == result_pos)
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
            ))
            // Compare mode: dual pane view with virtual scrolling
            .when(self.compare_mode, |parent| {
                let compare_tab_idx = self.compare_tab_index.unwrap_or(0);
                let active_doc = &self.tabs[self.active_tab].document;
                let compare_doc = &self.tabs[compare_tab_idx].document;
                let active_name = self.tabs[self.active_tab].display_name();
                let compare_name = self.tabs[compare_tab_idx].display_name();
                let max_len = active_doc.len().max(compare_doc.len());
                let compare_row_count = ui::row_count(max_len, self.bytes_per_row());

                // Calculate visible range specifically for compare mode using compare_row_count
                let compare_visible_range = ui::calculate_visible_range(
                    self.tab().scroll_offset,
                    content_height,
                    compare_row_count,
                    self.row_height(),
                );
                let compare_render_start = compare_visible_range.render_start;
                let compare_render_end = compare_visible_range.render_end;

                let (compare_top_spacer, compare_bottom_spacer) = ui::calculate_spacer_heights(
                    compare_render_start,
                    compare_render_end,
                    compare_row_count,
                    self.row_height(),
                );

                let font_size = self.settings.display.font_size;

                parent.child(
                    div()
                        .flex()
                        .flex_col()
                        .flex_1()
                        .pt_4()
                        .overflow_hidden()
                        // Header row with pane labels
                        .child(
                            div()
                                .flex()
                                .gap_2()
                                .pb_2()
                                .child(
                                    div()
                                        .flex_1()
                                        .text_sm()
                                        .text_color(rgb(0x4a9eff))
                                        .child(format!("Left: {}", active_name))
                                )
                                .child(
                                    div()
                                        .w(px(2.0))
                                )
                                .child(
                                    div()
                                        .flex_1()
                                        .text_sm()
                                        .text_color(rgb(0xff8c00))
                                        .child(format!("Right: {}", compare_name))
                                )
                        )
                        // Synchronized scrolling container with virtual scrolling
                        .child({
                            let cursor_pos = self.tab().cursor_position;
                            div()
                                .id("compare-scroll")
                                .flex_1()
                                .overflow_y_scroll()
                                .track_scroll(&self.tab().scroll_handle)
                                .child(
                                    div()
                                        .flex()
                                        .flex_col()
                                        // Top spacer for virtual scrolling
                                        .when(compare_render_start > 0, |d| d.child(div().h(compare_top_spacer)))
                                        // Row container
                                        .child(
                                            div()
                                                .flex()
                                                .flex_col()
                                                .children((compare_render_start..compare_render_end).map(|row| {
                                                    let start = row * self.bytes_per_row();
                                                    let address = ui::format_address(start);
                                                    let cursor_row = cursor_pos / self.bytes_per_row();
                                                    let is_cursor_row = row == cursor_row;

                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .mb_1()
                                                        // Address column
                                                        .child(
                                                            div()
                                                                .w(px(70.0))
                                                                .text_color(if is_cursor_row { rgb(0x4a9eff) } else { rgb(0x808080) })
                                                                .font_family("Monaco")
                                                                .text_size(px(font_size))
                                                                .child(address.clone())
                                                        )
                                                        // Left pane hex bytes
                                                        .child(
                                                            div()
                                                                .flex()
                                                                .gap_1()
                                                                .flex_1()
                                                                .font_family("Monaco")
                                                                .text_size(px(font_size))
                                                                .children((start..(start + self.bytes_per_row()).min(active_doc.len())).map(|byte_idx| {
                                                                    let byte = active_doc.get_byte(byte_idx).unwrap_or(0);
                                                                    let compare_byte = compare_doc.get_byte(byte_idx);
                                                                    let is_diff = compare_byte.map(|b| b != byte).unwrap_or(true);
                                                                    let is_cursor = byte_idx == cursor_pos;
                                                                    div()
                                                                        .when(is_cursor, |d| d.bg(rgb(0x4a9eff)).text_color(rgb(0x000000)))
                                                                        .when(!is_cursor, |d| d.text_color(if is_diff { rgb(0xff6666) } else { rgb(0x00ff00) }))
                                                                        .child(format!("{:02X}", byte))
                                                                }).collect::<Vec<_>>())
                                                        )
                                                        // Separator
                                                        .child(
                                                            div()
                                                                .w(px(2.0))
                                                                .bg(rgb(0x4a9eff))
                                                        )
                                                        // Right pane hex bytes
                                                        .child(
                                                            div()
                                                                .flex()
                                                                .gap_1()
                                                                .flex_1()
                                                                .font_family("Monaco")
                                                                .text_size(px(font_size))
                                                                .children((start..(start + self.bytes_per_row()).min(compare_doc.len())).map(|byte_idx| {
                                                                    let byte = compare_doc.get_byte(byte_idx).unwrap_or(0);
                                                                    let active_byte = active_doc.get_byte(byte_idx);
                                                                    let is_diff = active_byte.map(|b| b != byte).unwrap_or(true);
                                                                    let is_cursor = byte_idx == cursor_pos;
                                                                    div()
                                                                        .when(is_cursor, |d| d.bg(rgb(0xff8c00)).text_color(rgb(0x000000)))
                                                                        .when(!is_cursor, |d| d.text_color(if is_diff { rgb(0xff6666) } else { rgb(0x00ff00) }))
                                                                        .child(format!("{:02X}", byte))
                                                                }).collect::<Vec<_>>())
                                                        )
                                                }).collect::<Vec<_>>())
                                        )
                                        // Bottom spacer for virtual scrolling
                                        .when(compare_render_end < compare_row_count, |d| d.child(div().h(compare_bottom_spacer)))
                                )
                        })
                )
            })
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
                                            .child(ui::format_address(self.tab().cursor_position))
                                    )
                            )
                            .child(
                                // Current byte value (if valid position)
                                div()
                                    .flex()
                                    .gap_2()
                                    .text_color(rgb(0x808080))
                                    .when_some(self.tab().document.get_byte(self.tab().cursor_position), |el, byte| {
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
                            .when(self.tab().selection_start.is_some(), |el| {
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
                                // Text encoding selector
                                div()
                                    .flex()
                                    .gap_1()
                                    .items_center()
                                    .child(
                                        div()
                                            .text_color(rgb(0x808080))
                                            .child("Enc:")
                                    )
                                    .children(TextEncoding::all().iter().map(|enc| {
                                        let is_selected = self.text_encoding == *enc;
                                        let enc_copy = *enc;
                                        div()
                                            .px_1()
                                            .rounded_sm()
                                            .cursor_pointer()
                                            .bg(if is_selected { rgb(0x4a9eff) } else { rgb(0x333333) })
                                            .text_color(if is_selected { rgb(0x000000) } else { rgb(0x808080) })
                                            .hover(|h| h.bg(if is_selected { rgb(0x4a9eff) } else { rgb(0x444444) }))
                                            .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event: &gpui::MouseDownEvent, _window, cx| {
                                                this.set_encoding(enc_copy);
                                                cx.notify();
                                            }))
                                            .child(enc.label())
                                    }))
                            )
                            .child(
                                // File size (right aligned)
                                div()
                                    .flex_1()
                                    .text_right()
                                    .text_color(rgb(0x808080))
                                    .child(format!("Size: {}", ui::format_file_size(self.tab().document.len())))
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
                            .when(self.tab().is_searching, |el| {
                                let current = self.tab().search_progress.load(Ordering::Relaxed);
                                let total = self.tab().search_total;
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
                                                .child(format!("\"{}\"", self.tab().search_query))
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
                            .when(!self.tab().is_searching && self.tab().search_visible && !self.tab().search_query.is_empty(), |el| {
                                let result_count = self.tab().search_results.len();
                                let current_pos = self.tab().current_search_index.map(|i| i + 1).unwrap_or(0);
                                let mode_str = match self.tab().search_mode {
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
                                                .child(format!("\"{}\"", self.tab().search_query))
                                        )
                                        .child(
                                            div()
                                                .text_color(if result_count > 0 { rgb(0x00ff00) } else { rgb(0xff6666) })
                                                .child(if result_count > 0 {
                                                    if self.tab().search_truncated {
                                                        format!("{}/{}+ matches (truncated)", current_pos, result_count)
                                                    } else {
                                                        format!("{}/{} matches", current_pos, result_count)
                                                    }
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
                            .when(!self.tab().is_searching && !self.tab().search_visible && self.save_message.is_none(), |el| {
                                el.child(
                                    div()
                                        .text_color(rgb(0x606060))
                                        .child("Ready")
                                )
                            })
                    )
            )
            // Data Inspector Panel
            .when(self.inspector_visible, |parent| {
                let endian_label = match self.inspector_endian {
                    Endian::Little => "LE",
                    Endian::Big => "BE",
                };

                // Get inspector values
                let values = ui::DataInspectorValues::from_bytes(
                    |pos| self.tab().document.get_byte(pos),
                    self.tab().cursor_position,
                    self.tab().document.len(),
                    self.inspector_endian,
                );

                parent.child(
                    div()
                        .flex()
                        .flex_col()
                        .py_2()
                        .px_4()
                        .bg(rgb(0x1a1a1a))
                        .border_t_1()
                        .border_color(rgb(0x404040))
                        .text_sm()
                        .font_family("Monaco")
                        // Header row
                        .child(
                            div()
                                .flex()
                                .justify_between()
                                .pb_1()
                                .mb_1()
                                .border_b_1()
                                .border_color(rgb(0x333333))
                                .child(
                                    div()
                                        .flex()
                                        .gap_2()
                                        .child(
                                            div()
                                                .text_color(rgb(0x4a9eff))
                                                .child("Data Inspector")
                                        )
                                        .child(
                                            div()
                                                .text_color(rgb(0x808080))
                                                .child(format!("@ 0x{:08X}", self.tab().cursor_position))
                                        )
                                )
                                .child(
                                    div()
                                        .flex()
                                        .gap_2()
                                        .child(
                                            div()
                                                .text_color(rgb(0x808080))
                                                .child("Endian:")
                                        )
                                        .child(
                                            div()
                                                .text_color(rgb(0xff8c00))
                                                .child(endian_label)
                                        )
                                        .child(
                                            div()
                                                .text_color(rgb(0x606060))
                                                .child("(Ctrl+E)")
                                        )
                                )
                        )
                        // Values grid
                        .when_some(values, |el, vals| {
                            el.child(
                                div()
                                    .flex()
                                    .gap_8()
                                    // Integer column
                                    .child(
                                        div()
                                            .flex()
                                            .flex_col()
                                            .gap_1()
                                            // 8-bit values
                                            .child(
                                                div()
                                                    .flex()
                                                    .gap_2()
                                                    .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Int8:"))
                                                    .child(div().w(px(100.0)).text_right().text_color(rgb(0x00ff00)).child(format!("{}", vals.int8)))
                                            )
                                            .child(
                                                div()
                                                    .flex()
                                                    .gap_2()
                                                    .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("UInt8:"))
                                                    .child(div().w(px(100.0)).text_right().text_color(rgb(0x00ff00)).child(format!("{}", vals.uint8)))
                                            )
                                            // 16-bit values
                                            .when_some(vals.int16, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Int16:"))
                                                        .child(div().w(px(100.0)).text_right().text_color(rgb(0x00ff00)).child(format!("{}", v)))
                                                )
                                            })
                                            .when_some(vals.uint16, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("UInt16:"))
                                                        .child(div().w(px(100.0)).text_right().text_color(rgb(0x00ff00)).child(format!("{}", v)))
                                                )
                                            })
                                            // 32-bit values
                                            .when_some(vals.int32, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Int32:"))
                                                        .child(div().w(px(100.0)).text_right().text_color(rgb(0x00ff00)).child(format!("{}", v)))
                                                )
                                            })
                                            .when_some(vals.uint32, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("UInt32:"))
                                                        .child(div().w(px(100.0)).text_right().text_color(rgb(0x00ff00)).child(format!("{}", v)))
                                                )
                                            })
                                    )
                                    // 64-bit and float column
                                    .child(
                                        div()
                                            .flex()
                                            .flex_col()
                                            .gap_1()
                                            // 64-bit values
                                            .when_some(vals.int64, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Int64:"))
                                                        .child(div().w(px(180.0)).text_right().text_color(rgb(0x00ff00)).child(format!("{}", v)))
                                                )
                                            })
                                            .when_some(vals.uint64, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("UInt64:"))
                                                        .child(div().w(px(180.0)).text_right().text_color(rgb(0x00ff00)).child(format!("{}", v)))
                                                )
                                            })
                                            // Float values
                                            .when_some(vals.float32, |el, v| {
                                                let display = if v.is_nan() || v.is_infinite() {
                                                    format!("{}", v)
                                                } else {
                                                    format!("{:.6}", v)
                                                };
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Float32:"))
                                                        .child(div().w(px(180.0)).text_right().text_color(rgb(0xffff00)).child(display))
                                                )
                                            })
                                            .when_some(vals.float64, |el, v| {
                                                let display = if v.is_nan() || v.is_infinite() {
                                                    format!("{}", v)
                                                } else {
                                                    format!("{:.10}", v)
                                                };
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Float64:"))
                                                        .child(div().w(px(180.0)).text_right().text_color(rgb(0xffff00)).child(display))
                                                )
                                            })
                                    )
                                    // Hex column
                                    .child(
                                        div()
                                            .flex()
                                            .flex_col()
                                            .gap_1()
                                            .child(
                                                div()
                                                    .flex()
                                                    .gap_2()
                                                    .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Hex8:"))
                                                    .child(div().text_color(rgb(0x4a9eff)).child(format!("0x{:02X}", vals.uint8)))
                                            )
                                            .when_some(vals.uint16, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Hex16:"))
                                                        .child(div().text_color(rgb(0x4a9eff)).child(format!("0x{:04X}", v)))
                                                )
                                            })
                                            .when_some(vals.uint32, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Hex32:"))
                                                        .child(div().text_color(rgb(0x4a9eff)).child(format!("0x{:08X}", v)))
                                                )
                                            })
                                            .when_some(vals.uint64, |el, v| {
                                                el.child(
                                                    div()
                                                        .flex()
                                                        .gap_2()
                                                        .child(div().w(px(60.0)).text_color(rgb(0x808080)).child("Hex64:"))
                                                        .child(div().text_color(rgb(0x4a9eff)).child(format!("0x{:016X}", v)))
                                                )
                                            })
                                    )
                            )
                        })
                        // No data message
                        .when(self.tab().document.len() == 0, |el| {
                            el.child(
                                div()
                                    .text_color(rgb(0x808080))
                                    .child("No data available")
                            )
                        })
                )
            })
            // Compare tab selection dialog (modal overlay)
            .when(self.compare_selection_visible, |parent| {
                let tabs_info: Vec<(usize, String)> = self.tabs.iter().enumerate()
                    .filter(|(idx, _)| *idx != self.active_tab)
                    .map(|(idx, tab)| (idx, tab.display_name()))
                    .collect();

                parent.child(
                    div()
                        .absolute()
                        .top_0()
                        .left_0()
                        .right_0()
                        .bottom_0()
                        .bg(rgba(0x00000080))
                        .flex()
                        .items_center()
                        .justify_center()
                        .child(
                            div()
                                .bg(rgb(0x2a2a2a))
                                .border_1()
                                .border_color(rgb(0x4a9eff))
                                .rounded_md()
                                .p_4()
                                .min_w(px(300.0))
                                .flex()
                                .flex_col()
                                .gap_3()
                                .child(
                                    div()
                                        .text_lg()
                                        .text_color(rgb(0x4a9eff))
                                        .child("Select tab to compare")
                                )
                                .child(
                                    div()
                                        .text_sm()
                                        .text_color(rgb(0x808080))
                                        .child("Press 1-9 or click to select | Esc: cancel")
                                )
                                .children(
                                    tabs_info.into_iter().map(|(idx, name)| {
                                        let display_num = if idx < self.active_tab { idx + 1 } else { idx };
                                        div()
                                            .id(("compare-tab", idx))
                                            .flex()
                                            .gap_2()
                                            .px_3()
                                            .py_2()
                                            .rounded_md()
                                            .cursor_pointer()
                                            .bg(rgb(0x333333))
                                            .hover(|h| h.bg(rgb(0x4a9eff)))
                                            .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event, _window, cx| {
                                                this.select_compare_tab(idx);
                                                cx.notify();
                                            }))
                                            .child(
                                                div()
                                                    .text_color(rgb(0xff8c00))
                                                    .w(px(20.0))
                                                    .child(format!("{}", display_num))
                                            )
                                            .child(
                                                div()
                                                    .text_color(rgb(0xffffff))
                                                    .child(name)
                                            )
                                    }).collect::<Vec<_>>()
                                )
                        )
                )
            })
    }
}

fn main() {
    // Get command line arguments
    let args: Vec<String> = std::env::args().collect();

    Application::new().run(move |cx: &mut App| {
        // Initialize gpui-component theme for Scrollbar support
        gpui_component::theme::init(cx);

        // Load settings for window size
        let settings = Settings::load();
        let window_width = settings.window.width as f32;
        let window_height = settings.window.height as f32;
        let bounds = Bounds::centered(None, size(px(window_width), px(window_height)), cx);
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
