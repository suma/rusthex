#![windows_subsystem = "windows"]

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
//! - `cursor` - Cursor movement and navigation
//! - `bookmark` - Bookmark management
//! - `file_ops` - File operations (load, save, dialogs)
//! - `input` - Byte input handling (hex, ASCII)
//! - `inspector` - Data Inspector panel
//! - `tabs` - Tab management

mod bitmap;
mod bookmark;
mod compare;
mod config;
mod cursor;
mod document;
mod encoding;
mod file_ops;
mod input;
mod inspector;
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
    FontStyle, FontWeight, SharedString, Timer, Window, WindowBounds, WindowOptions, div, img,
    prelude::*, px, rgb, rgba, size,
};
use std::path::PathBuf;
use std::time::Duration;
use std::sync::atomic::Ordering;
use gpui_component::scroll::{Scrollbar, ScrollbarShow};

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
    cached_line_height_xs: f32,
    // Text encoding for ASCII column display
    text_encoding: TextEncoding,
    // Encoding dropdown open state
    encoding_dropdown_open: bool,
    // Cached monospace character width (calculated from font metrics in render)
    cached_char_width: f32,
    // Force close flag - when true, skip unsaved changes confirmation
    force_close: bool,
    // Bitmap visualization state
    bitmap_visible: bool,
    bitmap_width: usize,
    bitmap_color_mode: bitmap::BitmapColorMode,
    // Bitmap panel width (inline panel)
    bitmap_panel_width: f32,
    // Bitmap viewport indicator drag state
    bitmap_drag_start_y: Option<f32>,
    bitmap_drag_start_row: Option<usize>,
    // Bitmap independent scroll
    bitmap_scroll_handle: gpui::ScrollHandle,
    bitmap_scrollbar_state: gpui_component::scroll::ScrollbarState,
    // Bitmap image cache
    cached_bitmap_image: Option<std::sync::Arc<gpui::RenderImage>>,
    cached_bitmap_params: Option<BitmapCacheParams>,
}

/// Parameters used to determine if bitmap cache is valid
#[derive(Clone, PartialEq)]
struct BitmapCacheParams {
    scroll_start: usize,
    display_height: usize,
    bitmap_width: usize,
    pixel_size: u32,
    color_mode: bitmap::BitmapColorMode,
    cursor_position: usize,
    doc_len: usize,
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
            cached_line_height_xs: 15.0, // Default, will be updated in render()
            text_encoding: TextEncoding::default(),
            encoding_dropdown_open: false,
            cached_char_width: 8.4, // Default (14 * 0.6), will be updated in render()
            force_close: false,
            bitmap_visible: false,
            bitmap_width: bitmap::DEFAULT_BITMAP_WIDTH,
            bitmap_color_mode: bitmap::BitmapColorMode::default(),
            bitmap_panel_width: 520.0,
            bitmap_drag_start_y: None,
            bitmap_drag_start_row: None,
            bitmap_scroll_handle: gpui::ScrollHandle::new(),
            bitmap_scrollbar_state: gpui_component::scroll::ScrollbarState::default(),
            cached_bitmap_image: None,
            cached_bitmap_params: None,
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

        // Bookmark comment bar (conditional): ~35px total
        let bookmark_bar = if self.tab().bookmark_comment_editing {
            self.cached_line_height_sm + 20.0
        } else {
            0.0
        };

        base_header + tab_bar + search_bar + bookmark_bar
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

    /// Update bitmap image cache if parameters changed
    fn update_bitmap_cache(&mut self) {
        let doc_len = self.tab().document.len();
        let bitmap_width_pixels = self.bitmap_width;
        let bitmap_height = (doc_len + bitmap_width_pixels - 1) / bitmap_width_pixels;

        // Calculate pixel size
        let scrollbar_width = 12.0;
        let bitmap_area_width = self.bitmap_panel_width - 16.0 - scrollbar_width - 4.0;
        let pixel_size = (bitmap_area_width / bitmap_width_pixels as f32).max(1.0).floor();

        // Calculate canvas and display height
        let viewport_bounds = self.tab().scroll_handle.bounds();
        let bitmap_panel_height = f32::from(viewport_bounds.size.height);
        let panel_overhead = self.cached_line_height_sm
            + self.cached_line_height_xs * 2.0
            + 8.0 * 2.0
            + 8.0 * 3.0;
        let canvas_height = (bitmap_panel_height - panel_overhead).max(10.0 * pixel_size);
        let display_height = ((canvas_height / pixel_size) as usize).min(bitmap_height);

        // Calculate scroll position
        let bitmap_scroll_offset = self.bitmap_scroll_handle.offset();
        let bitmap_scroll_y: f32 = (-f32::from(bitmap_scroll_offset.y)).max(0.0);
        let bitmap_scroll_start = (bitmap_scroll_y / pixel_size) as usize;
        let bitmap_scroll_start = bitmap_scroll_start.min(bitmap_height.saturating_sub(display_height));

        let cursor_pos = self.tab().cursor_position;

        let current_params = BitmapCacheParams {
            scroll_start: bitmap_scroll_start,
            display_height,
            bitmap_width: bitmap_width_pixels,
            pixel_size: pixel_size as u32,
            color_mode: self.bitmap_color_mode,
            cursor_position: cursor_pos,
            doc_len,
        };

        // Check if cache is valid
        let cache_valid = self.cached_bitmap_params.as_ref()
            .map(|p| *p == current_params)
            .unwrap_or(false);

        if !cache_valid {
            // Create new bitmap image
            let doc = &self.tab().document;
            let new_image = bitmap::create_bitmap_image(
                |offset| doc.get_byte(offset),
                doc_len,
                bitmap_scroll_start,
                display_height,
                bitmap_width_pixels,
                pixel_size,
                self.bitmap_color_mode,
                cursor_pos,
            );
            self.cached_bitmap_image = Some(new_image);
            self.cached_bitmap_params = Some(current_params);
        }
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

    // Selection, file operations, cursor, and input methods are in separate modules:
    // - cursor.rs (movement)
    // - bookmark.rs (bookmarks)
    // - file_ops.rs (load/save)
    // - input.rs (selection, hex/ascii input)
    // - inspector.rs (data inspector)

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

    // Toggle bitmap visualization
    fn toggle_bitmap(&mut self) {
        self.bitmap_visible = !self.bitmap_visible;
        if self.bitmap_visible {
            self.save_message = Some(format!(
                "Bitmap: {} ({}px wide)",
                self.bitmap_color_mode.label(),
                self.bitmap_width
            ));
        } else {
            self.save_message = Some("Bitmap view closed".to_string());
        }
    }

    // Cycle bitmap color mode
    fn cycle_bitmap_color_mode(&mut self) {
        self.bitmap_color_mode = self.bitmap_color_mode.next();
        self.save_message = Some(format!("Bitmap mode: {}", self.bitmap_color_mode.label()));
    }

    // Increase bitmap width
    fn increase_bitmap_width(&mut self) {
        self.bitmap_width = bitmap::next_width(self.bitmap_width);
        self.save_message = Some(format!("Bitmap width: {}px", self.bitmap_width));
    }

    // Decrease bitmap width
    fn decrease_bitmap_width(&mut self) {
        self.bitmap_width = bitmap::prev_width(self.bitmap_width);
        self.save_message = Some(format!("Bitmap width: {}px", self.bitmap_width));
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
            family: self.settings.display.font_name.clone().into(),
            features: FontFeatures::default(),
            fallbacks: None,
            weight: FontWeight::default(),
            style: FontStyle::Normal,
        };
        let font_id = window.text_system().resolve_font(&font);
        let font_name = self.settings.display.font_name.clone();

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

        // Address column width: 8 characters for "00000000" format
        let address_width = self.cached_char_width * 8.0;

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

        // text_xs (12px) for small hints and labels
        let xs_size = px(12.0);
        let xs_ascent = window.text_system().ascent(font_id, xs_size);
        let xs_descent = window.text_system().descent(font_id, xs_size);
        self.cached_line_height_xs = f32::from(xs_ascent) + f32::from(xs_descent).abs();

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

        // Update bitmap image cache if bitmap is visible
        if self.bitmap_visible {
            self.update_bitmap_cache();
        }

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
                    this.bitmap_drag_start_y = None;
                    this.bitmap_drag_start_row = None;
                    cx.notify();
                }
            }))
            .on_mouse_move(cx.listener(|this, event: &gpui::MouseMoveEvent, _window, cx| {
                // Cancel drag if mouse button was released outside the window
                if this.is_dragging && !event.dragging() {
                    this.is_dragging = false;
                    this.drag_pane = None;
                    this.bitmap_drag_start_y = None;
                    this.bitmap_drag_start_row = None;
                    cx.notify();
                    return;
                }

                // Handle bitmap viewport indicator drag at root level
                if this.drag_pane == Some(EditPane::Bitmap) && this.is_dragging {
                    if let (Some(start_y), Some(start_row)) = (this.bitmap_drag_start_y, this.bitmap_drag_start_row) {
                        let mouse_y: f32 = event.position.y.into();
                        let delta_y = mouse_y - start_y;

                        // Convert pixel delta to bitmap rows
                        let bitmap_width = this.bitmap_width;
                        let bitmap_panel_width = this.bitmap_panel_width;
                        let pixel_size = ((bitmap_panel_width - 20.0) / bitmap_width as f32).max(1.0).min(4.0);
                        let delta_bitmap_rows = (delta_y / pixel_size) as isize;

                        // Convert bitmap rows to hex view rows
                        let bytes_per_row = this.bytes_per_row();
                        let delta_hex_rows = delta_bitmap_rows * bitmap_width as isize / bytes_per_row as isize;

                        // Calculate new scroll position
                        let new_row = if delta_hex_rows >= 0 {
                            start_row.saturating_add(delta_hex_rows as usize)
                        } else {
                            start_row.saturating_sub((-delta_hex_rows) as usize)
                        };

                        // Calculate scroll offset and apply
                        let doc_len = this.tab().document.len();
                        let total_rows = (doc_len + bytes_per_row - 1) / bytes_per_row;
                        let row_height = this.row_height();
                        let visible_rows = this.content_view_rows;

                        let new_y_offset = ui::calculate_scroll_offset(
                            new_row,
                            visible_rows,
                            total_rows,
                            row_height,
                        );
                        let current_offset = this.tab().scroll_handle.offset();
                        let new_offset = gpui::Point::new(current_offset.x, new_y_offset);
                        this.tab_mut().scroll_handle.set_offset(new_offset);
                        this.tab_mut().scroll_offset = new_y_offset;
                        cx.notify();
                    }
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
                                    EditPane::Bitmap => "HEX",
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
            // Bookmark comment editing bar
            .when(self.tab().bookmark_comment_editing, |parent| {
                let pos = self.tab().bookmark_comment_position;
                let comment_text = self.tab().bookmark_comment_text.clone();

                parent.child(
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
                                        .text_color(rgb(0x00bfff))
                                        .child(format!("0x{:08X}", pos))
                                )
                                .child(
                                    div()
                                        .text_sm()
                                        .text_color(rgb(0xffffff))
                                        .child(format!("Comment: {}", comment_text))
                                )
                        )
                        .child(
                            div()
                                .text_xs()
                                .text_color(rgb(0x808080))
                                .child("Enter: save | Esc: cancel | Backspace: delete")
                        )
                )
            })
            // Normal mode: single pane view (with optional bitmap panel)
            .when(!self.compare_mode, |parent| {
                let bitmap_visible = self.bitmap_visible;
                let bitmap_width_pixels = self.bitmap_width;
                let bitmap_color_mode = self.bitmap_color_mode;
                let bitmap_panel_width = self.bitmap_panel_width;

                // Hex view visible range (for bitmap viewport indicator)
                let hex_visible_start = render_start * self.bytes_per_row();
                let hex_visible_end = render_end * self.bytes_per_row();

                // Calculate minimum width for hex/ASCII content area
                let char_width = self.cached_char_width;
                let bytes_per_row = self.bytes_per_row();
                let address_width = char_width * 8.0 + 16.0; // 8 chars + bookmark indicator + padding
                let hex_column_width = bytes_per_row as f32 * char_width * 2.0
                    + (bytes_per_row - 1) as f32 * 4.0; // 2 chars per byte + gaps
                let ascii_column_width = bytes_per_row as f32 * char_width + 8.0; // Dynamic based on char_width + padding
                let gaps = 16.0 * 2.0; // gap_4 between columns
                let scrollbar_area = 24.0 + 16.0; // pr(24) + scrollbar width + margin
                let content_min_width = address_width + hex_column_width + ascii_column_width + gaps + scrollbar_area;

                parent.child(
                    // Outer container for hex view + bitmap (horizontal layout)
                    div()
                        .flex()
                        .flex_1()
                        .pt_4()
                        .gap_2()
                        .overflow_hidden()
                        .child(
                            // Main hex/ASCII content area with scrollbar (priority over bitmap)
                            div()
                                .flex()
                                .flex_1()
                                .flex_shrink_0() // Never shrink - ASCII view has priority
                                .min_w(px(content_min_width)) // Calculated minimum width
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
                                    this.bitmap_drag_start_y = None;
                                    this.bitmap_drag_start_row = None;
                                    cx.notify();
                                    return;
                                }

                                if !this.is_dragging || this.drag_pane.is_none() {
                                    return;
                                }

                                // Handle bitmap viewport indicator drag separately
                                if this.drag_pane == Some(EditPane::Bitmap) {
                                    if let (Some(start_y), Some(start_row)) = (this.bitmap_drag_start_y, this.bitmap_drag_start_row) {
                                        let mouse_y: f32 = event.position.y.into();
                                        let delta_y = mouse_y - start_y;

                                        // Convert pixel delta to bitmap rows
                                        let bitmap_width = this.bitmap_width;
                                        let bitmap_panel_width = this.bitmap_panel_width;
                                        let pixel_size = ((bitmap_panel_width - 20.0) / bitmap_width as f32).max(1.0).min(4.0);
                                        let delta_bitmap_rows = (delta_y / pixel_size) as isize;

                                        // Convert bitmap rows to hex view rows
                                        let bytes_per_row = this.bytes_per_row();
                                        let delta_hex_rows = delta_bitmap_rows * bitmap_width as isize / bytes_per_row as isize;

                                        // Calculate new scroll position
                                        let new_row = if delta_hex_rows >= 0 {
                                            start_row.saturating_add(delta_hex_rows as usize)
                                        } else {
                                            start_row.saturating_sub((-delta_hex_rows) as usize)
                                        };

                                        // Calculate scroll offset and apply
                                        let doc_len = this.tab().document.len();
                                        let total_rows = (doc_len + bytes_per_row - 1) / bytes_per_row;
                                        let row_height = this.row_height();
                                        let visible_rows = this.content_view_rows;

                                        let new_y_offset = ui::calculate_scroll_offset(
                                            new_row,
                                            visible_rows,
                                            total_rows,
                                            row_height,
                                        );
                                        let current_offset = this.tab().scroll_handle.offset();
                                        let new_offset = gpui::Point::new(current_offset.x, new_y_offset);
                                        this.tab_mut().scroll_handle.set_offset(new_offset);
                                        this.tab_mut().scroll_offset = new_y_offset;
                                        cx.notify();
                                    }
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
                                    Some(EditPane::Bitmap) => 0, // Bitmap drag handled separately above
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
                                    .font_family(&font_name)
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

                        // Check if this row has any bookmarks
                        let has_bookmark = self.tab().bookmarks.range(row_start..row_end).next().is_some();
                        // Check if any bookmark in this row has a comment
                        let has_bookmark_comment = self.tab().bookmarks.range(row_start..row_end)
                            .any(|(_, comment)| !comment.is_empty());

                        div()
                            .id(("hex-row", row))  // Stable ID for efficient diffing
                            .flex()
                            .flex_shrink_0()
                            .whitespace_nowrap()
                            .gap_4()
                            .mb_1()
                            .child(
                                // Address column - highlight cursor row, show bookmark indicator
                                div()
                                    .flex()
                                    .items_center()
                                    .w(px(address_width))
                                    .flex_shrink_0()
                                    .when(has_bookmark, |d| {
                                        d.child(
                                            div()
                                                .w(px(8.0))
                                                .h(px(8.0))
                                                .mr(px(4.0))
                                                .bg(if has_bookmark_comment { rgb(0x00ff88) } else { rgb(0x00bfff) })
                                                .rounded(px(4.0))
                                        )
                                    })
                                    .when(!has_bookmark, |d| {
                                        d.child(div().w(px(12.0))) // Spacer to align addresses
                                    })
                                    .child(
                                        div()
                                            .text_color(if is_cursor_row { rgb(0x4a9eff) } else { rgb(0x808080) })
                                            .child(address)
                                    )
                            )
                            .child(
                                // Hex column - always render bytes_per_row slots for consistent width
                                div()
                                    .flex()
                                    .gap_1()
                                    .children((0..bytes_per_row).map(|i| {
                                        let byte_idx = row_start + i;
                                        let has_data = byte_idx < document_len;

                                        // Check if this byte is bookmarked
                                        let is_bookmarked = has_data && has_bookmark && self.tab().bookmarks.contains_key(&byte_idx);
                                        let bookmark_has_comment = is_bookmarked && self.tab().bookmarks.get(&byte_idx).is_some_and(|c| !c.is_empty());

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
                                            // Bookmark underline indicator
                                            .when(is_bookmarked, |div| {
                                                div.border_b_2()
                                                    .border_color(if bookmark_has_comment { rgb(0x00ff88) } else { rgb(0x00bfff) })
                                            })
                                            .child(hex_str)
                                    }))
                            )
                            .child(
                                // ASCII column - each byte separately (using cached data + encoding)
                                div()
                                    .flex()
                                    .w(px(self.cached_char_width * bytes_per_row as f32))
                                    .children((row_start..row_end).enumerate().map(|(i, byte_idx)| {
                                        // Get display character from decoded data
                                        let display_char = decoded_chars.get(i).cloned().unwrap_or(DisplayChar::Invalid);
                                        let ascii_char = display_char.to_char();
                                        let is_continuation = display_char.is_continuation();

                                        // Check if this byte is bookmarked
                                        let is_bookmarked = has_bookmark && self.tab().bookmarks.contains_key(&byte_idx);
                                        let bookmark_has_comment = is_bookmarked && self.tab().bookmarks.get(&byte_idx).is_some_and(|c| !c.is_empty());

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
                                            // Bookmark underline indicator
                                            .when(is_bookmarked, |div| {
                                                div.border_b_2()
                                                    .border_color(if bookmark_has_comment { rgb(0x00ff88) } else { rgb(0x00bfff) })
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
                            // Add bookmark markers on scrollbar
                            .children({
                                let total_rows = ui::row_count(self.tab().document.len(), self.bytes_per_row());
                                let viewport_height = viewport_bounds.size.height;

                                if total_rows == 0 || viewport_height <= px(0.0) {
                                    Vec::new()
                                } else {
                                    self.tab().bookmarks.iter().map(|(&bookmark_pos, comment)| {
                                        let bookmark_row = bookmark_pos / self.bytes_per_row();
                                        let position_ratio = bookmark_row as f32 / total_rows as f32;
                                        let marker_position = viewport_height * position_ratio;
                                        let marker_color = if comment.is_empty() { rgb(0x00bfff) } else { rgb(0x00ff88) };

                                        div()
                                            .absolute()
                                            .left(px(0.0))
                                            .w(px(6.0))
                                            .h(px(6.0))
                                            .top(marker_position)
                                            .bg(marker_color)
                                            .rounded(px(3.0))
                                            .opacity(0.9)
                                    }).collect::<Vec<_>>()
                                }
                            })
                        )
                    ) // End of main hex/ASCII content area
                    // Bitmap panel (when visible)
                    .when(bitmap_visible, |container| {
                        let doc_len = self.tab().document.len();
                        let bitmap_height = (doc_len + bitmap_width_pixels - 1) / bitmap_width_pixels;

                        // Calculate pixel size based on panel width
                        // Account for scrollbar (12px) and gaps (8px padding + 4px gap)
                        let scrollbar_width = 12.0;
                        let bitmap_area_width = bitmap_panel_width - 16.0 - scrollbar_width - 4.0; // p_2 padding + gap
                        // Use floor to ensure consistent integer pixel size across all calculations
                        let pixel_size = (bitmap_area_width / bitmap_width_pixels as f32).max(1.0).floor();

                        // Calculate canvas height for bitmap display area
                        // Use viewport_height from scroll_handle bounds (same as hex view container height)
                        let viewport_bounds = self.tab().scroll_handle.bounds();
                        let bitmap_panel_height = f32::from(viewport_bounds.size.height);

                        // Subtract bitmap panel's internal elements from available height
                        let panel_overhead = self.cached_line_height_sm  // header
                            + self.cached_line_height_xs * 2.0           // controls hint + position info
                            + 8.0 * 2.0                                  // p_2 padding (top + bottom)
                            + 8.0 * 3.0;                                 // gap_2 (3 gaps between 4 children)
                        let canvas_height = (bitmap_panel_height - panel_overhead).max(10.0 * pixel_size);
                        let display_height = ((canvas_height / pixel_size) as usize).min(bitmap_height);

                        // Calculate scroll position from scroll handle
                        let bitmap_scroll_offset = self.bitmap_scroll_handle.offset();
                        let bitmap_scroll_y: f32 = (-f32::from(bitmap_scroll_offset.y)).max(0.0);
                        let bitmap_scroll_start = (bitmap_scroll_y / pixel_size) as usize;
                        let bitmap_scroll_start = bitmap_scroll_start.min(bitmap_height.saturating_sub(display_height));

                        container.child(
                            div()
                                .w(px(bitmap_panel_width))
                                .h(px(bitmap_panel_height))
                                .flex()
                                .flex_col()
                                .bg(rgb(0x252525))
                                .border_l_1()
                                .border_color(rgb(0x404040))
                                .p_2()
                                .gap_2()
                                .overflow_hidden()
                                .child(
                                    // Header
                                    div()
                                        .flex()
                                        .justify_between()
                                        .items_center()
                                        .child(
                                            div()
                                                .text_sm()
                                                .text_color(rgb(0x4a9eff))
                                                .child(format!("Bitmap ({})", bitmap_color_mode.label()))
                                        )
                                        .child(
                                            div()
                                                .text_sm()
                                                .text_color(rgb(0x808080))
                                                .child(format!("{}x{}", bitmap_width_pixels, bitmap_height))
                                        )
                                )
                                .child(
                                    // Controls hint
                                    div()
                                        .text_xs()
                                        .text_color(rgb(0x606060))
                                        .child("C: color | +/-: width | Ctrl+M: close")
                                )
                                .child(
                                    // Bitmap canvas with scrollbar (horizontal layout)
                                    div()
                                        .flex()
                                        .flex_1()
                                        .gap_1()
                                        .h(px(canvas_height))
                                        .child(
                                            // Scrollable bitmap canvas
                                            div()
                                                .id("bitmap-scroll-container")
                                                .relative()
                                                .flex_1()
                                                .h_full()
                                                .overflow_y_scroll()
                                                .track_scroll(&self.bitmap_scroll_handle)
                                                .child({
                                                    // Use cached bitmap image (updated in update_bitmap_cache)
                                                    let bitmap_image = self.cached_bitmap_image.clone()
                                                        .expect("Bitmap cache should be populated");

                                                    // Virtual height container for scrolling
                                                    // Capture values for click handler
                                                    let click_bitmap_width = bitmap_width_pixels;
                                                    let click_pixel_size = pixel_size;
                                                    let click_doc_len = doc_len;

                                                    div()
                                                        .id("bitmap-click-area")
                                                        .relative()
                                                        .w_full()
                                                        .h(px(bitmap_height as f32 * pixel_size))
                                                        .cursor_pointer()
                                                        .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, event: &gpui::MouseDownEvent, _window, cx| {
                                                            // Calculate byte offset from click position
                                                            // Get bitmap scroll container bounds to calculate relative position
                                                            let container_bounds = this.bitmap_scroll_handle.bounds();
                                                            let container_origin_x: f32 = container_bounds.origin.x.into();
                                                            let container_origin_y: f32 = container_bounds.origin.y.into();

                                                            // Calculate position relative to scroll container
                                                            let click_x: f32 = f32::from(event.position.x) - container_origin_x;
                                                            let click_y: f32 = f32::from(event.position.y) - container_origin_y;

                                                            // Account for bitmap scroll offset
                                                            let scroll_offset_y: f32 = (-f32::from(this.bitmap_scroll_handle.offset().y)).max(0.0);
                                                            let adjusted_y = click_y + scroll_offset_y;

                                                            let row = (adjusted_y / click_pixel_size) as usize;
                                                            let col = ((click_x / click_pixel_size).max(0.0) as usize).min(click_bitmap_width.saturating_sub(1));
                                                            let byte_offset = row * click_bitmap_width + col;
                                                            let byte_offset = byte_offset.min(click_doc_len.saturating_sub(1));

                                                            // Move cursor to clicked position and clear selection
                                                            this.tab_mut().cursor_position = byte_offset;
                                                            this.tab_mut().hex_nibble = ui::HexNibble::High;
                                                            this.tab_mut().selection_start = None;

                                                            // Scroll hex view to show cursor
                                                            let bytes_per_row = this.bytes_per_row();
                                                            let cursor_row = byte_offset / bytes_per_row;
                                                            let row_height = this.row_height();
                                                            let visible_rows = this.content_view_rows;
                                                            let doc_len = this.tab().document.len();
                                                            let total_rows = (doc_len + bytes_per_row - 1) / bytes_per_row;

                                                            // Center the cursor row in view
                                                            let target_row = cursor_row.saturating_sub(visible_rows / 2);
                                                            let new_y_offset = ui::calculate_scroll_offset(
                                                                target_row,
                                                                visible_rows,
                                                                total_rows,
                                                                row_height,
                                                            );
                                                            let current_offset = this.tab().scroll_handle.offset();
                                                            let new_offset = gpui::Point::new(current_offset.x, new_y_offset);
                                                            this.tab_mut().scroll_handle.set_offset(new_offset);
                                                            this.tab_mut().scroll_offset = new_y_offset;

                                                            cx.notify();
                                                        }))
                                                        .child(
                                                            // Bitmap image (positioned at scroll offset)
                                                            div()
                                                                .absolute()
                                                                .top(px(bitmap_scroll_start as f32 * pixel_size))
                                                                .left_0()
                                                                .child(img(bitmap_image))
                                                        )
                                                        // Viewport indicator overlay (thin rectangle)
                                                        .child({
                                                    // Calculate visible range position in bitmap coordinates
                                                    let visible_start_row = hex_visible_start / bitmap_width_pixels;
                                                    let visible_end_row = hex_visible_end.saturating_sub(1) / bitmap_width_pixels;

                                                    let top_px = visible_start_row as f32 * pixel_size;
                                                    let height_rows = visible_end_row.saturating_sub(visible_start_row) + 1;
                                                    let height_px = height_rows as f32 * pixel_size;
                                                    let width_px = bitmap_width_pixels as f32 * pixel_size;

                                                    let current_render_start = render_start;

                                                    div()
                                                        .id("bitmap-viewport-indicator")
                                                        .absolute()
                                                        .top(px(top_px))
                                                        .left_0()
                                                        .w(px(width_px))
                                                        .h(px(height_px))
                                                        .border_1()
                                                        .border_color(rgb(bitmap_color_mode.indicator_color()))
                                                        .cursor_grab()
                                                        .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, event: &gpui::MouseDownEvent, _window, cx| {
                                                            this.bitmap_drag_start_y = Some(event.position.y.into());
                                                            this.bitmap_drag_start_row = Some(current_render_start);
                                                            this.is_dragging = true;
                                                            this.drag_pane = Some(EditPane::Bitmap);
                                                            cx.notify();
                                                        }))
                                                })
                                                })
                                        )
                                        .child(
                                            // Scrollbar for bitmap
                                            div()
                                                .w(px(12.0))
                                                .h_full()
                                                .bg(rgb(0x2a2a2a))
                                                .child(
                                                    Scrollbar::vertical(&self.bitmap_scrollbar_state, &self.bitmap_scroll_handle)
                                                        .scrollbar_show(ScrollbarShow::Always)
                                                )
                                        )
                                )
                                .child(
                                    // Position info
                                    div()
                                        .text_xs()
                                        .text_color(rgb(0x808080))
                                        .child(format!("Cursor: row {}, col {} (0x{:08X})",
                                            self.tab().cursor_position / bitmap_width_pixels,
                                            self.tab().cursor_position % bitmap_width_pixels,
                                            self.tab().cursor_position
                                        ))
                                )
                        )
                    })
                ) // End of outer container
            })
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
                                                        .flex_shrink_0()
                                                        .whitespace_nowrap()
                                                        .gap_2()
                                                        .mb_1()
                                                        // Address column
                                                        .child(
                                                            div()
                                                                .w(px(address_width))
                                                                .flex_shrink_0()
                                                                .text_color(if is_cursor_row { rgb(0x4a9eff) } else { rgb(0x808080) })
                                                                .font_family(&font_name)
                                                                .text_size(px(font_size))
                                                                .child(address.clone())
                                                        )
                                                        // Left pane hex bytes
                                                        .child(
                                                            div()
                                                                .flex()
                                                                .gap_1()
                                                                .flex_1()
                                                                .font_family(&font_name)
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
                                                                .font_family(&font_name)
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
                    .font_family(&font_name)
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
                                // Text encoding dropdown selector
                                div()
                                    .relative()
                                    .flex()
                                    .gap_1()
                                    .items_center()
                                    .child(
                                        div()
                                            .text_color(rgb(0x808080))
                                            .child("Enc:")
                                    )
                                    .child(
                                        // Dropdown button
                                        div()
                                            .id("encoding-dropdown-button")
                                            .px_2()
                                            .rounded_sm()
                                            .cursor_pointer()
                                            .bg(rgb(0x333333))
                                            .text_color(rgb(0xffffff))
                                            .hover(|h| h.bg(rgb(0x444444)))
                                            .on_mouse_down(gpui::MouseButton::Left, cx.listener(|this, _event: &gpui::MouseDownEvent, _window, cx| {
                                                this.encoding_dropdown_open = !this.encoding_dropdown_open;
                                                cx.notify();
                                            }))
                                            .child(format!("{} \u{25BC}", self.text_encoding.label()))
                                    )
                                    // Dropdown menu (shown when open)
                                    .when(self.encoding_dropdown_open, |el| {
                                        el.child(
                                            div()
                                                .absolute()
                                                .bottom(px(24.0))
                                                .left_0()
                                                .bg(rgb(0x2a2a2a))
                                                .border_1()
                                                .border_color(rgb(0x555555))
                                                .rounded_md()
                                                .shadow_lg()
                                                .py_1()
                                                .min_w(px(100.0))
                                                .children(TextEncoding::all().iter().map(|enc| {
                                                    let is_selected = self.text_encoding == *enc;
                                                    let enc_copy = *enc;
                                                    div()
                                                        .id(SharedString::from(format!("enc-{}", enc.label())))
                                                        .px_3()
                                                        .py_1()
                                                        .cursor_pointer()
                                                        .bg(if is_selected { rgb(0x4a9eff) } else { rgb(0x2a2a2a) })
                                                        .text_color(if is_selected { rgb(0x000000) } else { rgb(0xcccccc) })
                                                        .hover(|h| h.bg(if is_selected { rgb(0x4a9eff) } else { rgb(0x3a3a3a) }))
                                                        .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event: &gpui::MouseDownEvent, _window, cx| {
                                                            this.set_encoding(enc_copy);
                                                            this.encoding_dropdown_open = false;
                                                            cx.notify();
                                                        }))
                                                        .child(if is_selected {
                                                            format!("\u{2713} {}", enc.label())
                                                        } else {
                                                            format!("  {}", enc.label())
                                                        })
                                                }))
                                        )
                                    })
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
                        .font_family(&font_name)
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
                let args_clone = args.clone();
                let entity = cx.new(|cx| {
                    // If file path is provided, load it; otherwise use sample data
                    let editor = if args_clone.len() > 1 {
                        let file_path = PathBuf::from(&args_clone[1]);
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
                    editor
                });

                // Set initial focus
                entity.update(cx, |_editor, cx| {
                    cx.focus_self(window);
                });

                // Set up window close confirmation for unsaved changes
                let weak_entity = entity.downgrade();
                window.on_window_should_close(cx, move |window, cx| {
                    if let Some(entity) = weak_entity.upgrade() {
                        let editor = entity.read(cx);

                        // If force_close is set, allow closing
                        if editor.force_close {
                            return true;
                        }

                        // If no unsaved changes, allow closing
                        if !editor.has_any_unsaved_changes() {
                            return true;
                        }

                        // Has unsaved changes - show confirmation dialog
                        let _ = entity.update(cx, |editor, cx| {
                            editor.confirm_close_with_unsaved(window, cx);
                        });

                        // Prevent closing for now - dialog will handle quit if confirmed
                        return false;
                    }
                    true
                });

                entity
            },
        )
        .unwrap();
        cx.activate(true);
    });
}
