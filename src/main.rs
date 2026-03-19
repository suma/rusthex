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

mod actions;
mod bitmap;
mod bookmark;
mod compare;
mod goto_address;
mod config;
mod cursor;
mod document;
mod encoding;
mod file_ops;
mod input;
mod inspector;
mod ipc;
mod keyboard;
mod log_panel;
mod pattern;
mod render;
mod render_cache;
mod search;
mod tab;
mod tabs;
mod theme;
mod ui;
mod vim;
#[cfg(target_os = "windows")]
mod windows_menu;

use bitmap::BitmapState;
use compare::CompareState;
use config::Settings;
use document::Document;
use gpui::{
    App, Application, Bounds, FocusHandle, Focusable, KeyBinding, Menu, MenuItem, OsAction,
    SharedString, SystemMenuType, Timer, WindowBounds, WindowOptions, prelude::*, point, px, size,
};
use render_cache::{CacheState, RenderCache};
pub use search::SearchMode;
use std::path::PathBuf;
use std::sync::Arc;
use std::sync::atomic::{AtomicU64, Ordering};
use std::time::Duration;
use tab::EditorTab;
pub use ui::{EditMode, EditPane, Endian, HexNibble, TextEncoding};

struct HexEditor {
    tabs: Vec<EditorTab>,
    active_tab: usize,
    settings: Settings,
    focus_handle: FocusHandle,
    log_panel: log_panel::LogPanel,
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
    // Pattern panel state
    pattern_panel_visible: bool,
    pattern_panel_width: f32,
    pattern_dropdown_open: bool,
    pattern_filter_query: String,
    pattern_filter_index: Option<usize>,
    // Search debounce generation counter
    search_debounce_gen: Arc<AtomicU64>,
    // Flag: user scrolled (mouse wheel) since last programmatic scroll
    user_scrolled: bool,
    // About dialog visibility
    about_visible: bool,
    // Cached window bounds (updated each render frame for save on quit)
    cached_window_bounds: Option<Bounds<gpui::Pixels>>,
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
        let initial_patterns = pattern::scan_hexpat_dir(&settings.pattern.hexpat_dir);
        let mut initial_tab = EditorTab::new();
        initial_tab.pattern.available_patterns = initial_patterns;
        let layout = settings.layout.clone();
        let mut editor = Self {
            tabs: vec![initial_tab],
            active_tab: 0,
            settings,
            focus_handle: cx.focus_handle(),
            log_panel: log_panel::LogPanel::new(),
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
            pattern_panel_visible: false,
            pattern_panel_width: 280.0,
            pattern_dropdown_open: false,
            pattern_filter_query: String::new(),
            pattern_filter_index: None,
            search_debounce_gen: Arc::new(AtomicU64::new(0)),
            user_scrolled: false,
            about_visible: false,
            cached_window_bounds: None,
        };
        editor.apply_layout(&layout);
        editor
    }

    /// Get bytes per row from settings
    fn bytes_per_row(&self) -> usize {
        self.settings.display.bytes_per_row
    }

    /// Check if any dropdown menu is open
    fn is_dropdown_open(&self) -> bool {
        self.encoding_dropdown_open || self.theme_dropdown_open || self.pattern_dropdown_open
    }

    /// Close all dropdown menus
    fn close_all_dropdowns(&mut self) {
        self.encoding_dropdown_open = false;
        self.theme_dropdown_open = false;
        self.pattern_dropdown_open = false;
    }

    /// Get row height (cached from font metrics calculation in render())
    fn row_height(&self) -> f64 {
        self.cached_row_height as f64
    }

    /// Calculate header height based on phi-based line heights matching gpui's actual rendering.
    /// Header content + Tab bar (conditional) + Search bar (conditional)
    fn calculate_header_height(&self) -> f32 {
        // Menu bar (Windows only): py_0.5(2+2) + text_sm + border_b_1(1) = sm + 5
        let menu_bar = if cfg!(target_os = "windows") {
            self.cached_line_height_sm + 5.0
        } else {
            0.0
        };

        // Header div: text_xl + text_sm + pb_4(16) + border_b_1(1)
        let base_header = self.cached_line_height_xl + self.cached_line_height_sm + 17.0;

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

        // Go to Address bar: py_2(8+8) + text_sm + text_xs + border_b_1(1) = sm + xs + 17
        let goto_address_bar = if self.tab().goto_address_visible {
            self.cached_line_height_sm + self.cached_line_height_xs + 17.0
        } else {
            0.0
        };

        menu_bar + base_header + tab_bar + search_bar + bookmark_bar + goto_address_bar
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

    /// Toggle pattern panel visibility
    fn toggle_pattern_panel(&mut self) {
        self.pattern_panel_visible = !self.pattern_panel_visible;
        self.pattern_dropdown_open = false;
        self.pattern_filter_query.clear();
        self.pattern_filter_index = None;
    }

    /// Select a pattern by index and run evaluation
    fn select_pattern(&mut self, index: usize) {
        self.tab_mut().pattern.selected_index = Some(index);
        self.pattern_dropdown_open = false;
        self.pattern_filter_query.clear();
        self.pattern_filter_index = None;
        self.run_pattern();
    }

    /// Get filtered patterns based on the current filter query.
    /// Returns a vector of (original_index, &PatternFileInfo) tuples.
    fn get_filtered_patterns(&self) -> Vec<(usize, &pattern::PatternFileInfo)> {
        let query = self.pattern_filter_query.to_lowercase();
        self.tab()
            .pattern
            .available_patterns
            .iter()
            .enumerate()
            .filter(|(_, pat)| query.is_empty() || pat.name.to_lowercase().contains(&query))
            .collect()
    }

    /// Run the currently selected pattern against the document
    fn run_pattern(&mut self) {
        let selected = self.tab().pattern.selected_index;
        if let Some(idx) = selected
            && idx < self.tab().pattern.available_patterns.len() {
                let pattern_path = self.tab().pattern.available_patterns[idx].path.clone();
                let hexpat_dir = std::path::PathBuf::from(&self.settings.pattern.hexpat_dir);
                let result = pattern::evaluate_pattern(
                    &pattern_path,
                    &self.tab().document,
                    &hexpat_dir,
                    &self.settings.pattern.include_dirs,
                );
                self.tab_mut().pattern.result = Some(result);
            }
    }

    /// Toggle a tree node expanded/collapsed
    fn toggle_pattern_node(&mut self, path: &str) {
        if self.tab().pattern.expanded_nodes.contains(path) {
            self.tab_mut().pattern.expanded_nodes.remove(path);
        } else {
            self.tab_mut()
                .pattern
                .expanded_nodes
                .insert(path.to_string());
        }
    }

    /// Add a message to the log panel
    fn log(&mut self, level: log_panel::LogLevel, message: impl Into<String>) {
        self.log_panel.push(level, message);
    }

    /// Toggle log panel visibility
    fn toggle_log_panel(&mut self) {
        self.log_panel.visible = !self.log_panel.visible;
    }

    // Selection, file operations, cursor, and input methods are in separate modules:
    // - cursor.rs (movement)
    // - bookmark.rs (bookmarks)
    // - file_ops.rs (load/save)
    // - input.rs (selection, hex/ascii input)
    // - inspector.rs (data inspector)

    /// Collect current layout state into settings
    fn collect_layout(&self) -> (config::WindowSettings, config::LayoutSettings) {
        let ws = if let Some(bounds) = self.cached_window_bounds {
            config::WindowSettings {
                width: f32::from(bounds.size.width).max(400.0) as u32,
                height: f32::from(bounds.size.height).max(300.0) as u32,
                x: Some(f32::from(bounds.origin.x) as i32),
                y: Some(f32::from(bounds.origin.y) as i32),
            }
        } else {
            self.settings.window.clone()
        };
        let ls = config::LayoutSettings {
            inspector_visible: self.inspector_visible,
            inspector_endian: match self.inspector_endian {
                Endian::Little => "little".to_string(),
                Endian::Big => "big".to_string(),
            },
            pattern_panel_visible: self.pattern_panel_visible,
            pattern_panel_width: self.pattern_panel_width,
            bitmap_visible: self.bitmap.visible,
            bitmap_panel_width: self.bitmap.panel_width,
            bitmap_color_mode: self.bitmap.color_mode.as_config_str().to_string(),
            bitmap_width: self.bitmap.width,
            text_encoding: self.text_encoding.as_config_str().to_string(),
            log_panel_visible: self.log_panel.visible,
            log_panel_height: self.log_panel.panel_height,
            log_panel_tab: match self.log_panel.active_tab {
                log_panel::BottomPanelTab::Log => "log".to_string(),
                log_panel::BottomPanelTab::Info => "info".to_string(),
            },
        };
        (ws, ls)
    }

    /// Apply layout settings from config
    fn apply_layout(&mut self, layout: &config::LayoutSettings) {
        self.inspector_visible = layout.inspector_visible;
        self.inspector_endian = match layout.inspector_endian.as_str() {
            "big" => Endian::Big,
            _ => Endian::Little,
        };
        self.pattern_panel_visible = layout.pattern_panel_visible;
        self.pattern_panel_width = layout.pattern_panel_width;
        self.bitmap.visible = layout.bitmap_visible;
        self.bitmap.panel_width = layout.bitmap_panel_width;
        self.bitmap.color_mode = bitmap::BitmapColorMode::from_config_str(&layout.bitmap_color_mode);
        self.bitmap.width = layout.bitmap_width;
        self.text_encoding = TextEncoding::from_label(&layout.text_encoding)
            .unwrap_or(TextEncoding::Ascii);
        self.log_panel.visible = layout.log_panel_visible;
        self.log_panel.panel_height = layout.log_panel_height;
        self.log_panel.active_tab = match layout.log_panel_tab.as_str() {
            "info" => log_panel::BottomPanelTab::Info,
            _ => log_panel::BottomPanelTab::Log,
        };
    }

    /// Save layout state to config file
    fn save_layout(&mut self) {
        let (ws, ls) = self.collect_layout();
        self.settings.window = ws;
        self.settings.layout = ls;
        let _ = self.settings.save();
    }

    /// Schedule a debounced search execution
    ///
    /// Waits for a short delay after the last keystroke before starting the search.
    /// If another keystroke arrives during the delay, the previous pending search is cancelled.
    pub fn schedule_search(&mut self, cx: &mut Context<Self>) {
        let generation = self.search_debounce_gen.fetch_add(1, Ordering::Relaxed) + 1;
        let gen_arc = Arc::clone(&self.search_debounce_gen);

        cx.spawn(async move |entity, cx| {
            Timer::after(Duration::from_millis(150)).await;

            // Only proceed if no newer keystroke has arrived
            if gen_arc.load(Ordering::Relaxed) != generation {
                return;
            }

            let _ = entity.update(cx, |editor, cx| {
                editor.perform_search();
                if editor.tab().is_searching {
                    editor.start_search_refresh_loop(cx);
                }
                cx.notify();
            });
        })
        .detach();
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
                && !self
                    .tab()
                    .render_cache
                    .row_needs_rebuild(row, bytes_per_row)
            {
                continue;
            }

            // Build row data
            let address_chars = crate::ui::address_chars(document_len);
            let row_data = RenderCache::build_row_data(
                row,
                bytes_per_row,
                document_len,
                cursor_position,
                selection_range,
                &self.tab().search_match_set,
                current_search_range,
                address_chars,
                |idx| self.tab().document.get_byte(idx),
                |idx| self.tab().document.is_modified(idx),
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

}

impl Focusable for HexEditor {
    fn focus_handle(&self, _cx: &App) -> FocusHandle {
        self.focus_handle.clone()
    }
}

/// Open a new editor window, optionally loading a file.
///
/// This is called from main() for the initial window, from on_reopen for Dock clicks,
/// and from the NewWindow action handler.
pub fn open_new_window(cx: &mut App, file_path: Option<PathBuf>) {
    use std::sync::atomic::AtomicBool;

    static IPC_STARTED: AtomicBool = AtomicBool::new(false);

    let settings = Settings::load();
    let w = px(settings.window.width as f32);
    let h = px(settings.window.height as f32);
    let bounds = match (settings.window.x, settings.window.y) {
        (Some(x), Some(y)) => Bounds {
            origin: point(px(x as f32), px(y as f32)),
            size: size(w, h),
        },
        _ => Bounds::centered(None, size(w, h), cx),
    };

    cx.open_window(
        WindowOptions {
            window_bounds: Some(WindowBounds::Windowed(bounds)),
            ..Default::default()
        },
        move |window, cx| {
            let entity = cx.new(|cx| {
                let mut editor = HexEditor::new(cx);
                if let Some(path) = file_path {
                    match editor.load_file(path.clone()) {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("Failed to load file: {}", e);
                        }
                    }
                }
                editor
            });

            // Install Win32 native menu bar (gpui's set_menus doesn't create HMENU on Windows)
            #[cfg(target_os = "windows")]
            windows_menu::install_native_menu(window, &build_menus());

            // Start IPC server once (first window only)
            if !IPC_STARTED.swap(true, Ordering::SeqCst) {
                entity.update(cx, |editor, cx| {
                    ipc::start_ipc_server(&cx.entity(), cx);
                    let _ = editor;
                });
            }

            // Detect file type and log the result
            entity.update(cx, |editor, cx| {
                if !editor.tab().document.is_empty() {
                    editor.detect_file_type(cx);
                }
            });

            // Set initial focus
            entity.update(cx, |_editor, cx| {
                cx.focus_self(window);
            });

            // Set up window close handler — close window only, don't quit app
            let weak_entity = entity.downgrade();
            window.on_window_should_close(cx, move |window, cx| {
                if let Some(entity) = weak_entity.upgrade() {
                    let editor = entity.read(cx);

                    if editor.force_close || !editor.has_any_unsaved_changes() {
                        entity.update(cx, |editor, _cx| {
                            editor.save_layout();
                        });
                        return true;
                    }

                    // Has unsaved changes - show confirmation dialog
                    entity.update(cx, |editor, cx| {
                        editor.confirm_close_window(window, cx);
                    });

                    return false;
                }
                true
            });

            entity
        },
    )
    .unwrap();
    cx.activate(true);
}

/// Show file picker and open the selected file in a new window.
/// Works at the App level so it can be called even with no windows open.
fn open_in_new_window_global(cx: &mut App) {
    let receiver = cx.prompt_for_paths(gpui::PathPromptOptions {
        files: true,
        directories: false,
        multiple: false,
        prompt: Some(SharedString::from("Open in New Window")),
    });

    cx.spawn(async move |cx| {
        if let Ok(Ok(Some(paths))) = receiver.await
            && let Some(path) = paths.into_iter().next() {
                cx.update(|cx| {
                    open_new_window(cx, Some(path));
                })
                .ok();
            }
    })
    .detach();
}

fn main() {
    // Get command line arguments
    let args: Vec<String> = std::env::args().collect();

    let app = Application::new();

    // Dock icon click when no windows are open — open a new empty window
    app.on_reopen(|cx| {
        open_new_window(cx, None);
    });

    app.run(move |cx: &mut App| {
        // Initialize gpui-component (theme, popup menu, etc.)
        gpui_component::init(cx);

        // Register key bindings (for menu shortcut display)
        cx.bind_keys([
            // File
            KeyBinding::new("cmd-n", actions::NewWindow, None),
            KeyBinding::new("cmd-o", actions::Open, None),
            KeyBinding::new("cmd-shift-o", actions::OpenInNewWindow, None),
            KeyBinding::new("cmd-s", actions::Save, None),
            KeyBinding::new("cmd-shift-s", actions::SaveAs, None),
            KeyBinding::new("cmd-t", actions::NewTab, None),
            KeyBinding::new("cmd-w", actions::CloseTab, None),
            KeyBinding::new("cmd-shift-w", actions::CloseWindow, None),
            KeyBinding::new("cmd-q", actions::Quit, None),
            // Edit
            KeyBinding::new("cmd-z", actions::Undo, None),
            KeyBinding::new("cmd-y", actions::Redo, None),
            KeyBinding::new("cmd-c", actions::Copy, None),
            KeyBinding::new("cmd-v", actions::Paste, None),
            KeyBinding::new("cmd-a", actions::SelectAll, None),
            KeyBinding::new("cmd-shift-i", actions::ToggleInsertMode, None),
            KeyBinding::new("cmd-g", actions::GoToAddress, None),
            // View
            KeyBinding::new("cmd-f", actions::ToggleSearch, None),
            KeyBinding::new("cmd-i", actions::ToggleInspector, None),
            KeyBinding::new("cmd-m", actions::ToggleBitmap, None),
            KeyBinding::new("cmd-p", actions::TogglePatternPanel, None),
            KeyBinding::new("cmd-k", actions::ToggleCompareMode, None),
            KeyBinding::new("cmd-shift-e", actions::CycleEncoding, None),
            KeyBinding::new("cmd-l", actions::ToggleLogPanel, None),
            // Search
            KeyBinding::new("f3", actions::FindNext, None),
            KeyBinding::new("shift-f3", actions::FindPrev, None),
            // Analyze
            KeyBinding::new("cmd-e", actions::AnalyzeSelection, None),
        ]);

        // Set native menu bar (works on macOS; on Windows, stored but not rendered by gpui)
        cx.set_menus(build_menus());

        // Global action handlers — work even when no window is open
        cx.on_action(|_: &actions::NewWindow, cx| {
            open_new_window(cx, None);
        });
        cx.on_action(|_: &actions::OpenInNewWindow, cx| {
            open_in_new_window_global(cx);
        });

        // Open initial window with optional file argument
        let file_path = if args.len() > 1 {
            Some(PathBuf::from(&args[1]))
        } else {
            None
        };
        open_new_window(cx, file_path);
    });
}

/// Build the application menu definition shared by macOS (NSMenu) and Windows (HMENU).
fn build_menus() -> Vec<Menu> {
    vec![
        // Application menu (macOS displays the first menu as the app-name menu)
        Menu {
            name: SharedString::from("Pheasant"),
            items: vec![
                MenuItem::action("About Pheasant", actions::About),
                MenuItem::separator(),
                MenuItem::os_submenu("Services", SystemMenuType::Services),
                MenuItem::separator(),
                MenuItem::action("Quit Pheasant", actions::Quit),
            ],
        },
        Menu {
            name: SharedString::from("File"),
            items: vec![
                MenuItem::action("New Window", actions::NewWindow),
                MenuItem::action("Open...", actions::Open),
                MenuItem::action("Open in New Window...", actions::OpenInNewWindow),
                MenuItem::separator(),
                MenuItem::action("Save", actions::Save),
                MenuItem::action("Save As...", actions::SaveAs),
                MenuItem::action("Save Selection As...", actions::SaveSelectionAs),
                MenuItem::separator(),
                MenuItem::action("New Tab", actions::NewTab),
                MenuItem::action("Close Tab", actions::CloseTab),
                MenuItem::action("Close Window", actions::CloseWindow),
            ],
        },
        Menu {
            name: SharedString::from("Edit"),
            items: vec![
                MenuItem::os_action("Undo", actions::Undo, OsAction::Undo),
                MenuItem::os_action("Redo", actions::Redo, OsAction::Redo),
                MenuItem::separator(),
                MenuItem::os_action("Copy", actions::Copy, OsAction::Copy),
                MenuItem::action("Copy as ASCII Text", actions::CopyAsAscii),
                MenuItem::action("Copy as Hex String", actions::CopyAsHexString),
                MenuItem::action("Copy as C Array", actions::CopyAsCArray),
                MenuItem::action("Copy as Python", actions::CopyAsPython),
                MenuItem::os_action("Paste", actions::Paste, OsAction::Paste),
                MenuItem::os_action("Select All", actions::SelectAll, OsAction::SelectAll),
                MenuItem::separator(),
                MenuItem::action("Toggle Insert Mode", actions::ToggleInsertMode),
                MenuItem::action("Go to Address...", actions::GoToAddress),
                MenuItem::separator(),
                MenuItem::action("Analyze Selection", actions::AnalyzeSelection),
            ],
        },
        Menu {
            name: SharedString::from("View"),
            items: vec![
                MenuItem::action("Search", actions::ToggleSearch),
                MenuItem::action("Data Inspector", actions::ToggleInspector),
                MenuItem::action("Bitmap", actions::ToggleBitmap),
                MenuItem::action("Pattern", actions::TogglePatternPanel),
                MenuItem::action("Compare", actions::ToggleCompareMode),
                MenuItem::separator(),
                MenuItem::action("Cycle Encoding", actions::CycleEncoding),
            ],
        },
    ]
}
