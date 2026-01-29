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
mod render;
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
use gpui::{
    App, Application, Bounds, Context, ExternalPaths, Focusable, FocusHandle, Font, FontFeatures,
    FontStyle, FontWeight, Timer, Window, WindowBounds, WindowOptions, div,
    prelude::*, px, rgb, size,
};
use std::path::PathBuf;
use std::time::Duration;

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
    cached_bitmap_params: Option<bitmap::BitmapCacheParams>,
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

        // Build render params for helper methods
        let params = render::RenderParams {
            font_name: font_name.clone(),
            address_width,
            render_start,
            render_end,
            row_count,
            top_spacer_height,
            bottom_spacer_height,
            content_height,
            viewport_bounds,
        };

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
            .child(self.render_header(&title))
            .when(self.tabs.len() > 1, |parent| {
                parent.child(self.render_tab_bar(cx))
            })
            .when(self.tab().search_visible, |parent| {
                parent.child(self.render_search_bar())
            })
            .when(self.tab().bookmark_comment_editing, |parent| {
                parent.child(self.render_bookmark_bar())
            })
            .when(!self.compare_mode, |parent| {
                parent.child(self.render_normal_mode(cx, &params))
            })
            .when(self.compare_mode, |parent| {
                parent.child(self.render_compare_mode(&params))
            })
            .child(self.render_status_bar(cx, &params.font_name))
            .when(self.inspector_visible, |parent| {
                parent.child(self.render_data_inspector(&params.font_name))
            })
            .when(self.compare_selection_visible, |parent| {
                parent.child(self.render_compare_dialog(cx))
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
