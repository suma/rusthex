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
mod theme;
mod ui;

use bitmap::BitmapState;
use compare::CompareState;
use config::Settings;
use document::Document;
use render_cache::{CacheState, RenderCache};
pub use search::SearchMode;
use tab::EditorTab;
pub use ui::{EditMode, EditPane, HexNibble, Endian, TextEncoding};
use gpui::{
    App, Application, Bounds, Focusable, FocusHandle, Timer,
    WindowBounds, WindowOptions, prelude::*, px, size,
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
    compare: CompareState,
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
    bitmap: BitmapState,
    // Color theme
    theme: theme::Theme,
    // Theme dropdown open state
    theme_dropdown_open: bool,
}

impl HexEditor {
    fn new(cx: &mut Context<Self>) -> Self {
        let settings = Settings::load();
        let inspector_endian = match settings.editor.default_endian {
            config::DefaultEndian::Little => Endian::Little,
            config::DefaultEndian::Big => Endian::Big,
        };
        let theme_name = theme::ThemeName::from_str(&settings.display.theme);
        let current_theme = theme::Theme::from_name(theme_name);
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
            compare: CompareState::new(),
            cached_row_height: 23.0, // Default (12 * phi + 4), will be updated in render()
            cached_line_height_xl: 32.0, // Default (20 * phi = 32), will be updated in render()
            cached_line_height_sm: 23.0, // Default (14 * phi = 23), will be updated in render()
            cached_line_height_xs: 19.0, // Default (12 * phi = 19), will be updated in render()
            text_encoding: TextEncoding::default(),
            encoding_dropdown_open: false,
            cached_char_width: 8.4, // Default (14 * 0.6), will be updated in render()
            force_close: false,
            bitmap: BitmapState::new(),
            theme: current_theme,
            theme_dropdown_open: false,
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

    /// Calculate header height based on phi-based line heights matching gpui's actual rendering.
    /// Header content + Tab bar (conditional) + Search bar (conditional)
    fn calculate_header_height(&self) -> f32 {
        // Header div: text_xl + 3 * text_sm + pb_4(16) + border_b_1(1)
        let base_header = self.cached_line_height_xl + 3.0 * self.cached_line_height_sm + 17.0;

        // Tab bar: py_1(4+4) + text_sm + py_1(4+4) + border_b_1(1) = sm + 17
        let tab_bar = if self.tabs.len() > 1 {
            self.cached_line_height_sm + 17.0
        } else {
            0.0
        };

        // Search bar: py_2(8+8) + text_sm + text_xs + border_b_1(1) = sm + xs + 17
        let search_bar = if self.tab().search_visible {
            self.cached_line_height_sm + self.cached_line_height_xs + 17.0
        } else {
            0.0
        };

        // Bookmark bar: py_2(8+8) + text_sm + text_xs + border_b_1(1) = sm + xs + 17
        let bookmark_bar = if self.tab().bookmark_comment_editing {
            self.cached_line_height_sm + self.cached_line_height_xs + 17.0
        } else {
            0.0
        };

        base_header + tab_bar + search_bar + bookmark_bar
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

    /// Set color theme
    fn set_theme(&mut self, name: theme::ThemeName) {
        self.theme = theme::Theme::from_name(name);
        self.invalidate_render_cache();
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
