use crate::HexEditor;
use crate::SearchMode;
use crate::actions;
use crate::encoding::{DisplayChar, decode_for_display};
use crate::keyboard;
use crate::pattern;
use crate::ui::{self, EditMode, EditPane, HexNibble, TextEncoding};
use gpui::{
    ExternalPaths, Font, FontFeatures, FontStyle, FontWeight, Pixels, SharedString, Window,
    deferred, div, img, prelude::*, px, rgb,
};
use gpui_component::menu::ContextMenuExt;
use gpui_component::scroll::{Scrollbar, ScrollbarShow};
use std::collections::HashSet;
use std::sync::atomic::Ordering;

/// Bundle of parameters computed in the render() preamble
pub(crate) struct RenderParams {
    pub font_name: String,
    pub address_width: f32,
    pub address_chars: usize,
    pub render_start: usize,
    pub render_end: usize,
    pub row_count: usize,
    pub top_spacer_height: Pixels,
    pub bottom_spacer_height: Pixels,
    pub content_height: Pixels,
    pub viewport_bounds: gpui::Bounds<Pixels>,
}

impl HexEditor {
    /// Header area (title, file info)
    pub(crate) fn render_header(&self, title: &str) -> impl IntoElement {
        let t = &self.theme;
        div()
            .flex()
            .flex_col()
            .pb_4()
            .border_b_1()
            .border_color(t.border_primary)
            .child(
                div()
                    .text_xl()
                    .text_color(t.text_primary)
                    .child(format!("{}{}",
                        title,
                        if self.tab().document.has_unsaved_changes() { " *" } else { "" }
                    ))
            )
            .child(
                div()
                    .text_sm()
                    .text_color(t.text_muted)
                    .child(format!("{} bytes{}",
                        self.tab().document.len(),
                        if self.tab().document.has_unsaved_changes() { " (modified)" } else { "" }
                    ))
            )
    }

    /// Menu bar rendered in gpui (Windows only).
    ///
    /// Win32 HMENU is invisible under gpui's DirectComposition topmost visual,
    /// so the menu bar is drawn as a gpui div and popups are shown via
    /// `TrackPopupMenu` on click.
    #[cfg(target_os = "windows")]
    pub(crate) fn render_menu_bar(&self, cx: &mut gpui::Context<Self>) -> impl IntoElement {
        let labels = crate::windows_menu::menu_labels();
        let bg_elevated = self.theme.bg_elevated;
        let border_primary = self.theme.border_primary;
        let text_muted = self.theme.text_muted;
        let text_primary = self.theme.text_primary;
        let bg_hover = self.theme.bg_hover;

        div()
            .flex()
            .items_center()
            .bg(bg_elevated)
            .border_b_1()
            .border_color(border_primary)
            .children(labels.into_iter().enumerate().map(move |(i, label)| {
                div()
                    .id(("menu", i))
                    .px_2()
                    .py_0p5()
                    .text_sm()
                    .text_color(text_muted)
                    .cursor_pointer()
                    .hover(move |h| h.bg(bg_hover).text_color(text_primary))
                    .on_mouse_down(
                        gpui::MouseButton::Left,
                        cx.listener(move |_this, _event: &gpui::MouseDownEvent, _window, cx| {
                            crate::windows_menu::show_popup_menu(i);
                            cx.notify();
                        }),
                    )
                    .child(label)
            }))
    }

    /// Tab bar (shown only when multiple tabs exist)
    pub(crate) fn render_tab_bar(&self, cx: &mut gpui::Context<Self>) -> impl IntoElement {
        let t = &self.theme;
        let dragging_tab = self.dragging_tab_index;
        let drop_target = self.tab_drop_target;
        let accent_primary = t.accent_primary;
        let bg_primary = t.bg_primary;
        let text_primary = t.text_primary;
        let border_primary = t.border_primary;
        let bg_elevated = t.bg_elevated;
        let text_muted = t.text_muted;
        let bg_hover = t.bg_hover;
        let text_diff = t.text_diff;
        let accent_success = t.accent_success;
        div()
            .flex()
            .gap_1()
            .py_1()
            .px_2()
            .bg(t.bg_surface)
            .border_b_1()
            .border_color(t.border_primary)
            .on_mouse_up(
                gpui::MouseButton::Left,
                cx.listener(|this, _event, _window, cx| {
                    // Complete the drag operation
                    if let (Some(from), Some(to)) = (this.dragging_tab_index, this.tab_drop_target)
                    {
                        if from != to {
                            this.reorder_tab(from, to);
                        }
                    }
                    this.dragging_tab_index = None;
                    this.tab_drop_target = None;
                    cx.notify();
                }),
            )
            .children(
                self.tabs
                    .iter()
                    .enumerate()
                    .map(|(idx, tab)| {
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
                                d.border_l_2().border_color(accent_primary)
                            })
                            // Dragging state (semi-transparent)
                            .when(is_being_dragged, |d| d.opacity(0.5))
                            .when(is_active && !is_being_dragged, |d| {
                                d.bg(bg_primary)
                                    .text_color(text_primary)
                                    .border_t_1()
                                    .border_l_1()
                                    .border_r_1()
                                    .border_color(border_primary)
                            })
                            .when(!is_active && !is_being_dragged, |d| {
                                d.bg(bg_elevated)
                                    .text_color(text_muted)
                                    .hover(|h| h.bg(bg_hover))
                            })
                            .on_mouse_down(
                                gpui::MouseButton::Left,
                                cx.listener(move |this, _event, _window, cx| {
                                    this.dragging_tab_index = Some(idx);
                                    this.switch_to_tab(idx);
                                    cx.notify();
                                }),
                            )
                            .on_mouse_move(cx.listener(
                                move |this, event: &gpui::MouseMoveEvent, _window, cx| {
                                    // Update drop target when dragging over tabs
                                    if this.dragging_tab_index.is_some() && event.dragging() {
                                        if this.tab_drop_target != Some(idx) {
                                            this.tab_drop_target = Some(idx);
                                            cx.notify();
                                        }
                                    }
                                },
                            ))
                            .child(tab_name)
                            .child(
                                // Close button
                                div()
                                    .id(("tab-close", idx))
                                    .text_xs()
                                    .text_color(text_muted)
                                    .hover(|h| h.text_color(text_diff))
                                    .cursor_pointer()
                                    .on_mouse_down(
                                        gpui::MouseButton::Left,
                                        cx.listener(
                                            move |this,
                                                  _event: &gpui::MouseDownEvent,
                                                  _window,
                                                  cx| {
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
                                            },
                                        ),
                                    )
                                    .child("×"),
                            )
                    })
                    .collect::<Vec<_>>(),
            )
            .child(
                // New tab button
                div()
                    .id("new-tab")
                    .px_2()
                    .py_1()
                    .text_sm()
                    .text_color(t.text_muted)
                    .cursor_pointer()
                    .hover(|h| h.text_color(accent_success))
                    .on_mouse_down(
                        gpui::MouseButton::Left,
                        cx.listener(|this, _event, _window, cx| {
                            this.new_tab();
                            cx.notify();
                        }),
                    )
                    .child("+"),
            )
    }

    /// Search bar
    pub(crate) fn render_search_bar(&self) -> impl IntoElement {
        let t = &self.theme;
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

        let warning_secondary = t.text_warning_secondary;
        let accent_success = t.accent_success;

        div()
            .flex()
            .flex_col()
            .py_2()
            .px_4()
            .bg(t.bg_elevated)
            .border_b_1()
            .border_color(t.border_primary)
            .child(
                div()
                    .flex()
                    .gap_4()
                    .items_center()
                    .child(
                        div()
                            .text_sm()
                            .text_color(t.text_primary)
                            .child(format!("Search ({}): {}", search_mode_label, self.tab().search_query))
                    )
                    .when(self.tab().is_searching, |d| {
                        d.child(
                            div()
                                .text_sm()
                                .text_color(t.text_warning)
                                .child("Searching...")
                        )
                    })
                    .when(!self.tab().is_searching && result_count > 0, |d| {
                        d.child(
                            div()
                                .text_sm()
                                .text_color(if self.tab().search_truncated { warning_secondary } else { accent_success })
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
                                .text_color(t.text_error)
                                .child("No matches")
                        )
                    })
            )
            .child(
                div()
                    .text_xs()
                    .text_color(t.text_muted)
                    .child("Type to search | Tab: switch mode | Enter/F3: next | Shift+F3: prev | Backspace: delete | Esc: close")
            )
    }

    /// Bookmark comment editing bar
    pub(crate) fn render_bookmark_bar(&self) -> impl IntoElement {
        let t = &self.theme;
        let pos = self.tab().bookmark_comment_position;
        let comment_text = self.tab().bookmark_comment_text.clone();

        div()
            .flex()
            .flex_col()
            .py_2()
            .px_4()
            .bg(t.bg_elevated)
            .border_b_1()
            .border_color(t.border_primary)
            .child(
                div()
                    .flex()
                    .gap_4()
                    .items_center()
                    .child(
                        div()
                            .text_sm()
                            .text_color(t.bookmark_plain)
                            .child(format!("0x{}", ui::format_address(pos, ui::address_chars(self.tab().document.len())))),
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(t.text_primary)
                            .child(format!("Comment: {}", comment_text)),
                    ),
            )
            .child(
                div()
                    .text_xs()
                    .text_color(t.text_muted)
                    .child("Enter: save | Esc: cancel | Backspace: delete"),
            )
    }

    /// Normal mode: hex/ASCII view with optional bitmap panel
    pub(crate) fn render_normal_mode(
        &self,
        cx: &mut gpui::Context<Self>,
        params: &RenderParams,
    ) -> impl IntoElement {
        let t = &self.theme;
        // Capture theme colors for closures
        let t_bookmark_comment = t.bookmark_comment;
        let t_bookmark_plain = t.bookmark_plain;
        let t_accent_primary = t.accent_primary;
        let t_text_muted = t.text_muted;
        let t_text_on_accent = t.text_on_accent;
        let t_bg_hover = t.bg_hover;
        let t_accent_success = t.accent_success;
        let t_search_current_bg = t.search_current_bg;
        let t_search_match_bg = t.search_match_bg;
        let t_bg_selection = t.bg_selection;
        let t_text_primary = t.text_primary;
        let t_accent_secondary = t.accent_secondary;
        let t_text_dim = t.text_dim;
        let t_text_modified = t.text_modified;
        let t_bg_elevated = t.bg_elevated;
        let t_bg_surface = t.bg_surface;
        let t_border_primary = t.border_primary;

        let bitmap_visible = self.bitmap.visible;
        let bitmap_width_pixels = self.bitmap.width;
        let bitmap_color_mode = self.bitmap.color_mode;
        let bitmap_panel_width = self.bitmap.panel_width;
        let render_start = params.render_start;
        let render_end = params.render_end;
        let row_count = params.row_count;
        let font_name = &params.font_name;
        let address_width = params.address_width;
        let top_spacer_height = params.top_spacer_height;
        let bottom_spacer_height = params.bottom_spacer_height;
        let viewport_bounds = params.viewport_bounds;

        // Hex view visible range (for bitmap viewport indicator)
        let hex_visible_start = render_start * self.bytes_per_row();
        let hex_visible_end = render_end * self.bytes_per_row();

        // Calculate minimum width for hex/ASCII content area
        let char_width = self.cached_char_width;
        let bytes_per_row = self.bytes_per_row();
        let address_col_width = char_width * params.address_chars as f32 + 16.0; // address chars + bookmark indicator + padding
        let hex_column_width =
            bytes_per_row as f32 * char_width * 2.0 + (bytes_per_row - 1) as f32 * 4.0; // 2 chars per byte + gaps
        let ascii_column_width = bytes_per_row as f32 * char_width + 8.0; // Dynamic based on char_width + padding
        let gaps = 16.0 * 2.0; // gap_4 between columns
        let scrollbar_area = 24.0 + 16.0; // pr(24) + scrollbar width + margin
        let content_min_width =
            address_col_width + hex_column_width + ascii_column_width + gaps + scrollbar_area;

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
                            .on_scroll_wheel(cx.listener(|this, _event: &gpui::ScrollWheelEvent, _window, _cx| {
                                this.user_scrolled = true;
                            }))
                            .context_menu({
                                let has_selection = self.has_selection();
                                move |menu, _window, _cx| {
                                    menu.menu("Copy", Box::new(actions::Copy))
                                        .menu("Copy as ASCII Text", Box::new(actions::CopyAsAscii))
                                        .menu("Copy as Hex String", Box::new(actions::CopyAsHexString))
                                        .menu("Copy as C Array", Box::new(actions::CopyAsCArray))
                                        .menu("Paste", Box::new(actions::Paste))
                                        .separator()
                                        .menu_with_enable("Save Selection As...", Box::new(actions::SaveSelectionAs), has_selection)
                                        .separator()
                                        .menu("Select All", Box::new(actions::SelectAll))
                                        .separator()
                                        .menu("Toggle Inspector", Box::new(actions::ToggleInspector))
                                        .menu("Toggle Log Panel", Box::new(actions::ToggleLogPanel))
                                        .menu("Clear Log", Box::new(actions::ClearLog))
                                }
                            })
                            .pr(px(24.0))
                            .child(
                                div()
                                    .flex()
                                    .flex_col()
                                    .font_family(font_name)
                                    .text_size(px(self.settings.display.font_size))
                                    // Top spacer for virtual scrolling
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
                                let addr = ui::format_address(row_start, params.address_chars);
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
                                    .flex_shrink_0()
                                    .when(has_bookmark, |d| {
                                        d.child(
                                            div()
                                                .size(px(8.0))
                                                .flex_shrink_0()
                                                .mr(px(4.0))
                                                .bg(if has_bookmark_comment { t_bookmark_comment } else { t_bookmark_plain })
                                                .rounded(px(4.0))
                                                .child("")
                                        )
                                    })
                                    .when(!has_bookmark, |d| {
                                        d.child(div().w(px(12.0)).flex_shrink_0()) // Spacer to align addresses
                                    })
                                    .child(
                                        div()
                                            .text_color(if is_cursor_row { t_accent_primary } else { t_text_muted })
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
                                        let (hex_str, is_cursor, is_selected, is_search_match, is_current_search, is_modified) =
                                            if has_data {
                                                match &row_data {
                                                    Some(data) if i < data.bytes.len() => {
                                                        let b = &data.bytes[i];
                                                        (format!("{:02X}", b.value), b.is_cursor, b.is_selected, b.is_search_match, b.is_current_search, b.is_modified)
                                                    }
                                                    _ => {
                                                        // Fallback: compute inline (shouldn't happen if cache is properly updated)
                                                        let byte = self.tab().document.get_byte(byte_idx).unwrap_or(0);
                                                        (format!("{:02X}", byte), false, false, false, false, false)
                                                    }
                                                }
                                            } else {
                                                // Empty slot - show cursor at doc_len in Insert mode
                                                let is_insert_cursor = byte_idx == self.tab().cursor_position
                                                    && self.tab().edit_mode == EditMode::Insert;
                                                ("  ".to_string(), is_insert_cursor, false, false, false, false)
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
                                                div.bg(t_accent_primary)
                                                    .text_color(t_text_on_accent)
                                            })
                                            .when(is_cursor && edit_pane == EditPane::Ascii, |div| {
                                                div.bg(t_bg_hover)
                                                    .text_color(t_accent_success)
                                            })
                                            .when(!is_cursor && is_current_search, |div| {
                                                div.bg(t_search_current_bg)
                                                    .text_color(t_text_on_accent)
                                            })
                                            .when(!is_cursor && !is_current_search && is_search_match, |div| {
                                                div.bg(t_search_match_bg)
                                                    .text_color(t_text_on_accent)
                                            })
                                            .when(!is_cursor && !is_current_search && !is_search_match && is_selected, |div| {
                                                div.bg(t_bg_selection)
                                                    .text_color(t_text_primary)
                                            })
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected && is_modified, |div| {
                                                div.text_color(t_text_modified)
                                            })
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected && !is_modified, |div| {
                                                div.text_color(t_accent_success)
                                            })
                                            // Bookmark underline indicator
                                            .when(is_bookmarked, |div| {
                                                div.border_b_2()
                                                    .border_color(if bookmark_has_comment { t_bookmark_comment } else { t_bookmark_plain })
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
                                        let (is_cursor, is_selected, is_search_match, is_current_search, is_modified) =
                                            match &row_data {
                                                Some(data) if i < data.bytes.len() => {
                                                    let b = &data.bytes[i];
                                                    (b.is_cursor, b.is_selected, b.is_search_match, b.is_current_search, b.is_modified)
                                                }
                                                _ => (false, false, false, false, false)
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
                                                div.bg(t_accent_secondary)
                                                    .text_color(t_text_on_accent)
                                            })
                                            .when(is_cursor && edit_pane == EditPane::Hex, |div| {
                                                div.bg(t_bg_hover)
                                                    .text_color(t_text_primary)
                                            })
                                            .when(!is_cursor && is_current_search, |div| {
                                                div.bg(t_search_current_bg)
                                                    .text_color(t_text_on_accent)
                                            })
                                            .when(!is_cursor && !is_current_search && is_search_match, |div| {
                                                div.bg(t_search_match_bg)
                                                    .text_color(t_text_on_accent)
                                            })
                                            .when(!is_cursor && !is_current_search && !is_search_match && is_selected, |div| {
                                                div.bg(t_bg_selection)
                                                    .text_color(t_text_primary)
                                            })
                                            // Continuation bytes shown in dim color
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected && is_continuation, |div| {
                                                div.text_color(t_text_dim)
                                            })
                                            // Modified (non-continuation) characters
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected && !is_continuation && is_modified, |div| {
                                                div.text_color(t_text_modified)
                                            })
                                            // Normal characters
                                            .when(!is_cursor && !is_current_search && !is_search_match && !is_selected && !is_continuation && !is_modified, |div| {
                                                div.text_color(t_text_primary)
                                            })
                                            // Bookmark underline indicator
                                            .when(is_bookmarked, |div| {
                                                div.border_b_2()
                                                    .border_color(if bookmark_has_comment { t_bookmark_comment } else { t_bookmark_plain })
                                            })
                                            .child(ascii_char.to_string())
                                    }))
                                    // In Insert mode, show cursor when it is at doc_len and belongs to this row
                                    .when(
                                        self.tab().edit_mode == EditMode::Insert
                                            && self.tab().cursor_position == document_len
                                            && self.tab().cursor_position >= row_start
                                            && self.tab().cursor_position < row_start + bytes_per_row,
                                        |el| {
                                            let cursor_el = if edit_pane == EditPane::Ascii {
                                                div().bg(t_accent_secondary).text_color(t_text_on_accent).child(" ")
                                            } else {
                                                div().bg(t_bg_hover).text_color(t_text_primary).child(" ")
                                            };
                                            el.child(cursor_el)
                                        }
                                    )
                            )
                    }))
                                    // Bottom spacer for virtual scrolling
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
                            .bg(t_bg_elevated)
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
                                                t_search_current_bg
                                            } else {
                                                t_search_match_bg
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
                                        let marker_color = if comment.is_empty() { t_bookmark_plain } else { t_bookmark_comment };

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
                let bitmap_scroll_offset = self.bitmap.scroll_handle.offset();
                let bitmap_scroll_y: f32 = (-f32::from(bitmap_scroll_offset.y)).max(0.0);
                let bitmap_scroll_start = (bitmap_scroll_y / pixel_size) as usize;
                let bitmap_scroll_start = bitmap_scroll_start.min(bitmap_height.saturating_sub(display_height));

                container.child(
                    div()
                        .w(px(bitmap_panel_width))
                        .h(px(bitmap_panel_height))
                        .flex()
                        .flex_col()
                        .bg(t_bg_surface)
                        .border_l_1()
                        .border_color(t_border_primary)
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
                                        .text_color(t_accent_primary)
                                        .child(format!("Bitmap ({})", bitmap_color_mode.label()))
                                )
                                .child(
                                    div()
                                        .text_sm()
                                        .text_color(t_text_muted)
                                        .child(format!("{}x{}", bitmap_width_pixels, bitmap_height))
                                )
                        )
                        .child(
                            // Controls hint
                            div()
                                .text_xs()
                                .text_color(t_text_dim)
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
                                        .track_scroll(&self.bitmap.scroll_handle)
                                        .child({
                                            // Use cached bitmap image (updated in update_bitmap_cache)
                                            let bitmap_image = self.bitmap.cached_image.clone()
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
                                                    let container_bounds = this.bitmap.scroll_handle.bounds();
                                                    let container_origin_x: f32 = container_bounds.origin.x.into();
                                                    let container_origin_y: f32 = container_bounds.origin.y.into();

                                                    // Calculate position relative to scroll container
                                                    let click_x: f32 = f32::from(event.position.x) - container_origin_x;
                                                    let click_y: f32 = f32::from(event.position.y) - container_origin_y;

                                                    // Account for bitmap scroll offset
                                                    let scroll_offset_y: f32 = (-f32::from(this.bitmap.scroll_handle.offset().y)).max(0.0);
                                                    let adjusted_y = click_y + scroll_offset_y;

                                                    let row = (adjusted_y / click_pixel_size) as usize;
                                                    let col = ((click_x / click_pixel_size).max(0.0) as usize).min(click_bitmap_width.saturating_sub(1));
                                                    let byte_offset = row * click_bitmap_width + col;
                                                    let byte_offset = byte_offset.min(click_doc_len.saturating_sub(1));

                                                    // Move cursor to clicked position and clear selection
                                                    this.push_cursor_history();
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
                                                    this.bitmap.drag_start_y = Some(event.position.y.into());
                                                    this.bitmap.drag_start_row = Some(current_render_start);
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
                                        .bg(t_bg_elevated)
                                        .child(
                                            Scrollbar::vertical(&self.bitmap.scrollbar_state, &self.bitmap.scroll_handle)
                                                .scrollbar_show(ScrollbarShow::Always)
                                        )
                                )
                        )
                        .child(
                            // Position info
                            div()
                                .text_xs()
                                .text_color(t_text_muted)
                                .child(format!("Cursor: row {}, col {} (0x{})",
                                    self.tab().cursor_position / bitmap_width_pixels,
                                    self.tab().cursor_position % bitmap_width_pixels,
                                    ui::format_address(self.tab().cursor_position, ui::address_chars(self.tab().document.len()))
                                ))
                        )
                )
            })
            // Pattern panel (when visible)
            .when(self.pattern_panel_visible, |container| {
                container.child(self.render_pattern_panel(cx))
            })
    }

    /// Pattern panel: tree view of decoded binary structure
    pub(crate) fn render_pattern_panel(&self, cx: &mut gpui::Context<Self>) -> impl IntoElement {
        let t = &self.theme;
        let t_bg_surface = t.bg_surface;
        let t_border_primary = t.border_primary;
        let t_accent_primary = t.accent_primary;
        let t_text_primary = t.text_primary;
        let t_text_muted = t.text_muted;
        let t_text_dim = t.text_dim;
        let t_accent_success = t.accent_success;
        let t_bg_hover = t.bg_hover;
        let t_bg_elevated = t.bg_elevated;
        let t_text_error = t.text_error;

        let panel_width = self.pattern_panel_width;
        let viewport_bounds = self.tab().scroll_handle.bounds();
        let panel_height = f32::from(viewport_bounds.size.height);
        let dropdown_open = self.pattern_dropdown_open;

        // Build panel content
        let mut panel = div()
            .w(px(panel_width))
            .h(px(panel_height))
            .flex()
            .flex_col()
            .bg(t_bg_surface)
            .border_l_1()
            .border_color(t_border_primary)
            .overflow_hidden();

        // Header: "Pattern" + close button
        panel = panel.child(
            div()
                .flex()
                .justify_between()
                .items_center()
                .px_2()
                .py_1()
                .border_b_1()
                .border_color(t_border_primary)
                .child(
                    div()
                        .text_sm()
                        .text_color(t_accent_primary)
                        .child("Pattern"),
                )
                .child(
                    div()
                        .id("pattern-close")
                        .text_sm()
                        .text_color(t_text_muted)
                        .cursor_pointer()
                        .hover(|s| s.text_color(t_text_primary))
                        .on_mouse_down(
                            gpui::MouseButton::Left,
                            cx.listener(|this, _event, _window, cx| {
                                this.pattern_panel_visible = false;
                                cx.notify();
                            }),
                        )
                        .child("✕"),
                ),
        );

        // Pattern dropdown selector
        let selected_name = self
            .tab()
            .pattern
            .selected_index
            .and_then(|idx| self.tab().pattern.available_patterns.get(idx))
            .map(|p| p.name.clone())
            .unwrap_or_else(|| "Select pattern...".to_string());

        panel = panel.child(
            div()
                .px_2()
                .py_1()
                .border_b_1()
                .border_color(t_border_primary)
                .child(
                    div()
                        .id("pattern-dropdown-trigger")
                        .flex()
                        .justify_between()
                        .items_center()
                        .px_2()
                        .py_1()
                        .bg(t_bg_elevated)
                        .rounded_sm()
                        .cursor_pointer()
                        .hover(|s| s.bg(t_bg_hover))
                        .text_sm()
                        .text_color(t_text_primary)
                        .on_mouse_down(
                            gpui::MouseButton::Left,
                            cx.listener(|this, _event, _window, cx| {
                                this.pattern_dropdown_open = !this.pattern_dropdown_open;
                                if this.pattern_dropdown_open {
                                    this.pattern_filter_query.clear();
                                    let filtered = this.get_filtered_patterns();
                                    this.pattern_filter_index =
                                        if filtered.is_empty() { None } else { Some(0) };
                                }
                                cx.notify();
                            }),
                        )
                        .child(selected_name)
                        .child(
                            div()
                                .text_xs()
                                .text_color(t_text_muted)
                                .child(if dropdown_open { "▲" } else { "▼" }),
                        ),
                )
                .when(dropdown_open, |dropdown| {
                    let filter_query = self.pattern_filter_query.clone();
                    let filter_index = self.pattern_filter_index;
                    let filtered_patterns = self.get_filtered_patterns();

                    let mut list = div()
                        .id("pattern-dropdown-list")
                        .absolute()
                        .w(px(panel_width - 16.0))
                        .bg(t_bg_elevated)
                        .border_1()
                        .border_color(t_border_primary)
                        .rounded_sm()
                        .overflow_y_scroll()
                        .max_h(px(200.0));

                    // Filter input field
                    list = list.child(
                        div()
                            .px_2()
                            .py_1()
                            .bg(t_bg_elevated)
                            .border_b_1()
                            .border_color(t_border_primary)
                            .text_sm()
                            .text_color(t_text_primary)
                            .child(format!("Filter: {}_", filter_query)),
                    );

                    if filtered_patterns.is_empty() {
                        let message = if self.tab().pattern.available_patterns.is_empty() {
                            "No .hexpat files found"
                        } else {
                            "No matching patterns"
                        };
                        list = list.child(
                            div()
                                .px_2()
                                .py_1()
                                .bg(t_bg_elevated)
                                .text_xs()
                                .text_color(t_text_dim)
                                .child(message),
                        );
                    } else {
                        for (list_idx, (original_idx, pat)) in filtered_patterns.iter().enumerate()
                        {
                            let name = pat.name.clone();
                            let original_idx = *original_idx;
                            let is_selected =
                                self.tab().pattern.selected_index == Some(original_idx);
                            let is_highlighted = filter_index == Some(list_idx);
                            list = list.child(
                                div()
                                    .id(SharedString::from(format!(
                                        "pattern-item-{}",
                                        original_idx
                                    )))
                                    .px_2()
                                    .py_1()
                                    .bg(if is_highlighted {
                                        t_bg_hover
                                    } else {
                                        t_bg_elevated
                                    })
                                    .text_sm()
                                    .text_color(if is_selected {
                                        t_accent_primary
                                    } else {
                                        t_text_primary
                                    })
                                    .cursor_pointer()
                                    .hover(|s| s.bg(t_bg_hover))
                                    .on_mouse_down(
                                        gpui::MouseButton::Left,
                                        cx.listener(move |this, _event, _window, cx| {
                                            this.select_pattern(original_idx);
                                            cx.notify();
                                        }),
                                    )
                                    .child(name),
                            );
                        }
                    }

                    dropdown.child(deferred(list).with_priority(100))
                }),
        );

        // Tree view or status
        match &self.tab().pattern.result {
            Some(pattern::PatternResult::Err(err)) => {
                let path_str = err.path.display().to_string();
                let err_msg = err.message.clone();
                panel = panel.child(
                    div()
                        .id("pattern-error")
                        .flex_1()
                        .p_2()
                        .overflow_y_scroll()
                        .child(
                            div()
                                .text_xs()
                                .text_color(t_text_error)
                                .child(format!("{}: {}", path_str, err_msg)),
                        ),
                );
            }
            Some(pattern::PatternResult::Ok(nodes)) => {
                let expanded = &self.tab().pattern.expanded_nodes;
                let total_visible = pattern::count_visible_nodes(nodes, expanded, "");

                // Build flattened tree items
                let mut tree_items: Vec<(String, &pattern_lang::PatternNode, usize)> = Vec::new();
                Self::flatten_tree(nodes, expanded, "", 0, &mut tree_items);

                let mut tree_view = div()
                    .id("pattern-tree-scroll")
                    .flex_1()
                    .overflow_y_scroll()
                    .track_scroll(&self.tab().pattern.scroll_handle)
                    .px_1()
                    .py_1();

                for (path, node, depth) in &tree_items {
                    let indent = *depth as f32 * 16.0;
                    let has_children = !node.children.is_empty();
                    let is_expanded = expanded.contains(path);
                    let path_clone = path.clone();

                    let display_name = node
                        .attributes
                        .display_name
                        .as_deref()
                        .unwrap_or(&node.name);

                    // Build node line
                    let mut node_div = div()
                        .id(SharedString::from(format!("pn-{}", path)))
                        .flex()
                        .items_center()
                        .gap_1()
                        .pl(px(indent))
                        .pr_1()
                        .py(px(1.0))
                        .text_xs()
                        .cursor_pointer()
                        .hover(|s| s.bg(t_bg_hover));

                    // Click handler for expand/collapse
                    if has_children {
                        node_div = node_div.on_mouse_down(
                            gpui::MouseButton::Left,
                            cx.listener(move |this, _event, _window, cx| {
                                this.toggle_pattern_node(&path_clone);
                                cx.notify();
                            }),
                        );
                    }

                    // Expand/collapse indicator
                    if has_children {
                        node_div = node_div.child(
                            div()
                                .text_xs()
                                .text_color(t_text_dim)
                                .w(px(12.0))
                                .child(if is_expanded { "▼" } else { "▶" }),
                        );
                    } else {
                        node_div = node_div.child(div().w(px(12.0)));
                    }

                    // Name
                    node_div = node_div.child(
                        div()
                            .text_color(t_text_primary)
                            .child(display_name.to_string()),
                    );

                    // Value (for leaf nodes)
                    match &node.value {
                        pattern_lang::PatternValue::Struct
                        | pattern_lang::PatternValue::Union
                        | pattern_lang::PatternValue::Array
                        | pattern_lang::PatternValue::Bitfield
                        | pattern_lang::PatternValue::LazyStructArray { .. } => {
                            // Show type name for container types
                            node_div = node_div.child(
                                div()
                                    .text_color(t_text_muted)
                                    .child(format!(" [{}]", node.type_name)),
                            );
                        }
                        _ => {
                            // Show value for leaf types
                            node_div = node_div.child(
                                div()
                                    .text_color(t_accent_success)
                                    .child(format!(" = {}", pattern::format_value(&node.value))),
                            );
                            node_div = node_div.child(
                                div()
                                    .text_color(t_text_muted)
                                    .child(format!(" [{}]", node.type_name)),
                            );
                        }
                    }

                    // Offset
                    node_div = node_div.child(
                        div()
                            .text_color(t_text_dim)
                            .child(format!(" @ 0x{:X}", node.offset)),
                    );

                    tree_view = tree_view.child(node_div);
                }

                panel = panel.child(tree_view);

                // Status bar
                panel = panel.child(
                    div()
                        .px_2()
                        .py_1()
                        .border_t_1()
                        .border_color(t_border_primary)
                        .text_xs()
                        .text_color(t_text_dim)
                        .child(format!(
                            "{} nodes ({} visible)",
                            Self::count_all_nodes(nodes),
                            total_visible
                        )),
                );
            }
            None => {
                panel = panel.child(
                    div().flex_1().flex().items_center().justify_center().child(
                        div()
                            .text_xs()
                            .text_color(t_text_dim)
                            .child("Select a pattern to decode"),
                    ),
                );
            }
        }

        panel
    }

    /// Flatten tree nodes for rendering (pre-order traversal respecting expanded state)
    fn flatten_tree<'a>(
        nodes: &'a [pattern_lang::PatternNode],
        expanded: &HashSet<String>,
        prefix: &str,
        depth: usize,
        out: &mut Vec<(String, &'a pattern_lang::PatternNode, usize)>,
    ) {
        for node in nodes {
            if node.attributes.hidden {
                continue;
            }
            let path = if prefix.is_empty() {
                node.name.clone()
            } else {
                format!("{}.{}", prefix, node.name)
            };
            out.push((path.clone(), node, depth));
            if !node.children.is_empty() && expanded.contains(&path) {
                Self::flatten_tree(&node.children, expanded, &path, depth + 1, out);
            }
        }
    }

    /// Count all nodes in a tree (including hidden ones)
    fn count_all_nodes(nodes: &[pattern_lang::PatternNode]) -> usize {
        let mut count = 0;
        for node in nodes {
            count += 1;
            count += Self::count_all_nodes(&node.children);
        }
        count
    }

    /// Compare mode: dual pane view with virtual scrolling
    pub(crate) fn render_compare_mode(&self, params: &RenderParams) -> impl IntoElement {
        let t = &self.theme;
        let t_compare_left = t.compare_left;
        let t_compare_right = t.compare_right;
        let t_compare_separator = t.compare_separator;
        let t_accent_primary = t.accent_primary;
        let t_text_muted = t.text_muted;
        let t_text_on_accent = t.text_on_accent;
        let t_text_diff = t.text_diff;
        let t_accent_success = t.accent_success;

        let compare_tab_idx = self.compare.tab_index.unwrap_or(0);
        let active_doc = &self.tabs[self.active_tab].document;
        let compare_doc = &self.tabs[compare_tab_idx].document;
        let active_name = self.tabs[self.active_tab].display_name();
        let compare_name = self.tabs[compare_tab_idx].display_name();
        let max_len = active_doc.len().max(compare_doc.len());
        let compare_row_count = ui::row_count(max_len, self.bytes_per_row());

        // Calculate visible range specifically for compare mode using compare_row_count
        let compare_visible_range = ui::calculate_visible_range(
            self.tab().scroll_offset,
            params.content_height,
            compare_row_count,
            self.row_height(),
            self.tab().scroll_logical_row,
        );
        let compare_render_start = compare_visible_range.render_start;
        let compare_render_end = compare_visible_range.render_end;

        let (compare_top_spacer, compare_bottom_spacer) = ui::calculate_spacer_heights(
            compare_render_start,
            compare_render_end,
            compare_row_count,
            self.row_height(),
            self.tab().scroll_offset,
            compare_visible_range.buffer_before,
        );

        let font_size = self.settings.display.font_size;
        let font_name = &params.font_name;
        let address_width = params.address_width;

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
                            .text_color(t.compare_left)
                            .child(format!("Left: {}", active_name)),
                    )
                    .child(div().w(px(2.0)))
                    .child(
                        div()
                            .flex_1()
                            .text_sm()
                            .text_color(t.compare_right)
                            .child(format!("Right: {}", compare_name)),
                    ),
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
                            .when(compare_render_start > 0, |d| {
                                d.child(div().h(compare_top_spacer))
                            })
                            // Row container
                            .child(
                                div().flex().flex_col().children(
                                    (compare_render_start..compare_render_end)
                                        .map(|row| {
                                            let start = row * self.bytes_per_row();
                                            let address = ui::format_address(start, params.address_chars);
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
                                                        .text_color(if is_cursor_row {
                                                            t_accent_primary
                                                        } else {
                                                            t_text_muted
                                                        })
                                                        .font_family(font_name)
                                                        .text_size(px(font_size))
                                                        .child(address.clone()),
                                                )
                                                // Left pane hex bytes
                                                .child(
                                                    div()
                                                        .flex()
                                                        .gap_1()
                                                        .flex_1()
                                                        .font_family(font_name)
                                                        .text_size(px(font_size))
                                                        .children(
                                                            (start
                                                                ..(start + self.bytes_per_row())
                                                                    .min(active_doc.len()))
                                                                .map(|byte_idx| {
                                                                    let byte = active_doc
                                                                        .get_byte(byte_idx)
                                                                        .unwrap_or(0);
                                                                    let compare_byte = compare_doc
                                                                        .get_byte(byte_idx);
                                                                    let is_diff = compare_byte
                                                                        .map(|b| b != byte)
                                                                        .unwrap_or(true);
                                                                    let is_cursor =
                                                                        byte_idx == cursor_pos;
                                                                    div()
                                                                        .when(is_cursor, |d| {
                                                                            d.bg(t_compare_left)
                                                                                .text_color(
                                                                                t_text_on_accent,
                                                                            )
                                                                        })
                                                                        .when(!is_cursor, |d| {
                                                                            d.text_color(
                                                                                if is_diff {
                                                                                    t_text_diff
                                                                                } else {
                                                                                    t_accent_success
                                                                                },
                                                                            )
                                                                        })
                                                                        .child(format!(
                                                                            "{:02X}",
                                                                            byte
                                                                        ))
                                                                })
                                                                .collect::<Vec<_>>(),
                                                        ),
                                                )
                                                // Separator
                                                .child(div().w(px(2.0)).bg(t_compare_separator))
                                                // Right pane hex bytes
                                                .child(
                                                    div()
                                                        .flex()
                                                        .gap_1()
                                                        .flex_1()
                                                        .font_family(font_name)
                                                        .text_size(px(font_size))
                                                        .children(
                                                            (start
                                                                ..(start + self.bytes_per_row())
                                                                    .min(compare_doc.len()))
                                                                .map(|byte_idx| {
                                                                    let byte = compare_doc
                                                                        .get_byte(byte_idx)
                                                                        .unwrap_or(0);
                                                                    let active_byte = active_doc
                                                                        .get_byte(byte_idx);
                                                                    let is_diff = active_byte
                                                                        .map(|b| b != byte)
                                                                        .unwrap_or(true);
                                                                    let is_cursor =
                                                                        byte_idx == cursor_pos;
                                                                    div()
                                                                        .when(is_cursor, |d| {
                                                                            d.bg(t_compare_right)
                                                                                .text_color(
                                                                                t_text_on_accent,
                                                                            )
                                                                        })
                                                                        .when(!is_cursor, |d| {
                                                                            d.text_color(
                                                                                if is_diff {
                                                                                    t_text_diff
                                                                                } else {
                                                                                    t_accent_success
                                                                                },
                                                                            )
                                                                        })
                                                                        .child(format!(
                                                                            "{:02X}",
                                                                            byte
                                                                        ))
                                                                })
                                                                .collect::<Vec<_>>(),
                                                        ),
                                                )
                                        })
                                        .collect::<Vec<_>>(),
                                ),
                            )
                            // Bottom spacer for virtual scrolling
                            .when(compare_render_end < compare_row_count, |d| {
                                d.child(div().h(compare_bottom_spacer))
                            }),
                    )
            })
    }

    /// Status bar (cursor position, byte value, selection, search status, etc.)
    pub(crate) fn render_status_bar(
        &self,
        cx: &mut gpui::Context<Self>,
        font_name: &String,
    ) -> impl IntoElement {
        let t = &self.theme;
        let t_accent_primary = t.accent_primary;
        let t_text_on_accent = t.text_on_accent;
        let t_bg_elevated = t.bg_elevated;
        let t_bg_hover_tertiary = t.bg_hover_tertiary;
        let t_bg_hover_secondary = t.bg_hover_secondary;
        let t_text_secondary = t.text_secondary;
        let t_text_muted = t.text_muted;
        let t_border_dropdown = t.border_dropdown;
        let t_accent_success = t.accent_success;
        let t_text_diff = t.text_diff;
        let t_text_warning = t.text_warning;

        // Determine current theme name for display
        use crate::theme::ThemeName;
        let current_theme_name = ThemeName::all()
            .iter()
            .find(|name| {
                let candidate = crate::theme::Theme::from_name(**name);
                candidate.bg_primary == self.theme.bg_primary
                    && candidate.accent_primary == self.theme.accent_primary
            })
            .copied()
            .unwrap_or(ThemeName::Dark);

        div()
            .flex()
            .flex_col()
            .py_1()
            .px_4()
            .bg(t.bg_surface)
            .border_t_1()
            .border_color(t.border_primary)
            .text_sm()
            .font_family(font_name)
            // First line: cursor position, byte value, selection
            .child(
                div()
                    .flex()
                    .gap_4()
                    .py_1()
                    .child(
                        // Edit mode indicator
                        div()
                            .text_color(match self.tab().edit_mode {
                                EditMode::Overwrite => t.text_primary,
                                EditMode::Insert => t.text_insert_mode,
                            })
                            .child(match self.tab().edit_mode {
                                EditMode::Overwrite => "OVR",
                                EditMode::Insert => "INS",
                            })
                    )
                    .child(
                        // Cursor position
                        div()
                            .flex()
                            .gap_2()
                            .text_color(t.text_muted)
                            .child("Offset:")
                            .child(
                                div()
                                    .text_color(t.accent_success)
                                    .child(ui::format_address(self.tab().cursor_position, ui::address_chars(self.tab().document.len())))
                            )
                    )
                    .child(
                        // Current byte value (if valid position)
                        div()
                            .flex()
                            .gap_2()
                            .text_color(t.text_muted)
                            .when_some(self.tab().document.get_byte(self.tab().cursor_position), |el, byte| {
                                el.child("Value:")
                                    .child(
                                        div()
                                            .text_color(t.accent_primary)
                                            .child(ui::format_byte_hex(byte))
                                    )
                                    .child(
                                        div()
                                            .text_color(t.text_primary)
                                            .child(format!("({})", ui::format_byte_dec(byte)))
                                    )
                                    .child(
                                        div()
                                            .text_color(t.accent_secondary)
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
                                .text_color(t.text_muted)
                                .child("Selection:")
                                .child(
                                    div()
                                        .text_color(t.text_warning)
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
                                    .text_color(t.text_muted)
                                    .child("Enc:")
                            )
                            .child(
                                // Dropdown button
                                div()
                                    .id("encoding-dropdown-button")
                                    .px_2()
                                    .rounded_sm()
                                    .cursor_pointer()
                                    .bg(t.bg_hover)
                                    .text_color(t.text_primary)
                                    .hover(|h| h.bg(t_bg_hover_tertiary))
                                    .on_mouse_down(gpui::MouseButton::Left, cx.listener(|this, _event: &gpui::MouseDownEvent, _window, cx| {
                                        this.encoding_dropdown_open = !this.encoding_dropdown_open;
                                        this.theme_dropdown_open = false;
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
                                        .bg(t_bg_elevated)
                                        .border_1()
                                        .border_color(t_border_dropdown)
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
                                                .bg(if is_selected { t_accent_primary } else { t_bg_elevated })
                                                .text_color(if is_selected { t_text_on_accent } else { t_text_secondary })
                                                .hover(|h| h.bg(if is_selected { t_accent_primary } else { t_bg_hover_secondary }))
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
                        // Theme dropdown selector
                        div()
                            .relative()
                            .flex()
                            .gap_1()
                            .items_center()
                            .child(
                                div()
                                    .text_color(t.text_muted)
                                    .child("Theme:")
                            )
                            .child(
                                // Dropdown button
                                div()
                                    .id("theme-dropdown-button")
                                    .px_2()
                                    .rounded_sm()
                                    .cursor_pointer()
                                    .bg(t.bg_hover)
                                    .text_color(t.text_primary)
                                    .hover(|h| h.bg(t_bg_hover_tertiary))
                                    .on_mouse_down(gpui::MouseButton::Left, cx.listener(|this, _event: &gpui::MouseDownEvent, _window, cx| {
                                        this.theme_dropdown_open = !this.theme_dropdown_open;
                                        this.encoding_dropdown_open = false;
                                        cx.notify();
                                    }))
                                    .child(format!("{} \u{25BC}", current_theme_name.label()))
                            )
                            // Dropdown menu (shown when open)
                            .when(self.theme_dropdown_open, |el| {
                                el.child(
                                    div()
                                        .absolute()
                                        .bottom(px(24.0))
                                        .left_0()
                                        .bg(t_bg_elevated)
                                        .border_1()
                                        .border_color(t_border_dropdown)
                                        .rounded_md()
                                        .shadow_lg()
                                        .py_1()
                                        .min_w(px(100.0))
                                        .children(ThemeName::all().iter().map(|name| {
                                            let is_selected = *name == current_theme_name;
                                            let name_copy = *name;
                                            div()
                                                .id(SharedString::from(format!("theme-{}", name.label())))
                                                .px_3()
                                                .py_1()
                                                .cursor_pointer()
                                                .bg(if is_selected { t_accent_primary } else { t_bg_elevated })
                                                .text_color(if is_selected { t_text_on_accent } else { t_text_secondary })
                                                .hover(|h| h.bg(if is_selected { t_accent_primary } else { t_bg_hover_secondary }))
                                                .on_mouse_down(gpui::MouseButton::Left, cx.listener(move |this, _event: &gpui::MouseDownEvent, _window, cx| {
                                                    this.set_theme(name_copy);
                                                    this.theme_dropdown_open = false;
                                                    cx.notify();
                                                }))
                                                .child(if is_selected {
                                                    format!("\u{2713} {}", name.label())
                                                } else {
                                                    format!("  {}", name.label())
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
                            .text_color(t.text_muted)
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
                    .border_color(t.border_secondary)
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
                                .text_color(t_text_warning)
                                .child("Searching...")
                                .child(
                                    div()
                                        .text_color(t_text_muted)
                                        .child(format!("\"{}\"", self.tab().search_query))
                                )
                                .child(
                                    div()
                                        .text_color(t_accent_primary)
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
                                        .text_color(t_text_muted)
                                        .child(format!("Search ({}):", mode_str))
                                )
                                .child(
                                    div()
                                        .text_color(t_accent_primary)
                                        .child(format!("\"{}\"", self.tab().search_query))
                                )
                                .child(
                                    div()
                                        .text_color(if result_count > 0 { t_accent_success } else { t_text_diff })
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
            )
    }

    /// Log panel for displaying accumulated status messages
    pub(crate) fn render_log_panel(&self, cx: &mut gpui::Context<Self>, font_name: &String) -> impl IntoElement {
        let t = &self.theme;
        let entry_count = self.log_panel.entries.len();
        let panel_height = self.log_panel.panel_height;

        div()
            .flex()
            .flex_col()
            // Resize handle at top
            .child(
                div()
                    .id("log-resize-handle")
                    .h(px(4.0))
                    .w_full()
                    .cursor_row_resize()
                    .on_mouse_down(
                        gpui::MouseButton::Left,
                        cx.listener(|this, event: &gpui::MouseDownEvent, _window, _cx| {
                            let mouse_y: f32 = event.position.y.into();
                            this.log_panel.drag_start_y = Some(mouse_y);
                            this.log_panel.drag_start_height = Some(this.log_panel.panel_height);
                        }),
                    ),
            )
            .py_1()
            .px_4()
            .bg(t.bg_secondary)
            .border_t_1()
            .border_color(t.border_primary)
            .text_xs()
            .font_family(font_name)
            // Header row
            .child(
                div()
                    .flex()
                    .justify_between()
                    .pb_1()
                    .mb_1()
                    .border_b_1()
                    .border_color(t.border_secondary)
                    .child(
                        div()
                            .flex()
                            .gap_2()
                            .child(div().text_color(t.accent_primary).child("Log"))
                            .child(
                                div()
                                    .text_color(t.text_muted)
                                    .child(format!("{} entries", entry_count)),
                            ),
                    )
                    .child(
                        div()
                            .id("clear-log-btn")
                            .text_color(t.text_muted)
                            .hover(|s| s.text_color(t.accent_primary))
                            .cursor_pointer()
                            .child("Clear")
                            .on_click(cx.listener(|this, _event, _window, cx| {
                                this.log_panel.clear();
                                cx.notify();
                            })),
                    ),
            )
            // Log entries with dynamic height
            .child(
                div()
                    .id("log-entries")
                    .flex()
                    .flex_col()
                    .h(px(panel_height))
                    .overflow_y_scroll()
                    .children(self.log_panel.entries.iter().map(|entry| {
                        let color = match entry.level {
                            crate::log_panel::LogLevel::Info => t.text_secondary,
                            crate::log_panel::LogLevel::Warning => t.text_warning,
                            crate::log_panel::LogLevel::Error => t.text_error,
                        };
                        let ts = self.log_panel.format_timestamp(entry.timestamp);
                        div()
                            .text_color(color)
                            .child(format!("[{}] {}", ts, entry.message))
                    })),
            )
    }

    /// Data Inspector panel
    pub(crate) fn render_data_inspector(&self, font_name: &String) -> impl IntoElement {
        let t = &self.theme;
        let endian_label = match self.inspector_endian {
            crate::Endian::Little => "LE",
            crate::Endian::Big => "BE",
        };

        // Get inspector values
        let values = ui::DataInspectorValues::from_bytes(
            |pos| self.tab().document.get_byte(pos),
            self.tab().cursor_position,
            self.tab().document.len(),
            self.inspector_endian,
        );

        div()
            .flex()
            .flex_col()
            .py_2()
            .px_4()
            .bg(t.bg_secondary)
            .border_t_1()
            .border_color(t.border_primary)
            .text_sm()
            .font_family(font_name)
            // Header row
            .child(
                div()
                    .flex()
                    .justify_between()
                    .pb_1()
                    .mb_1()
                    .border_b_1()
                    .border_color(t.border_secondary)
                    .child(
                        div()
                            .flex()
                            .gap_2()
                            .child(div().text_color(t.accent_primary).child("Data Inspector"))
                            .child(
                                div()
                                    .text_color(t.text_muted)
                                    .child(format!("@ 0x{}", ui::format_address(self.tab().cursor_position, ui::address_chars(self.tab().document.len())))),
                            ),
                    )
                    .child(
                        div()
                            .flex()
                            .gap_2()
                            .child(div().text_color(t.text_muted).child("Endian:"))
                            .child(div().text_color(t.accent_secondary).child(endian_label))
                            .child(div().text_color(t.text_dim).child("(Ctrl+E)")),
                    ),
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
                                        .child(
                                            div()
                                                .w(px(60.0))
                                                .text_color(t.text_muted)
                                                .child("Int8:"),
                                        )
                                        .child(
                                            div()
                                                .w(px(100.0))
                                                .text_right()
                                                .text_color(t.accent_success)
                                                .child(format!("{}", vals.int8)),
                                        ),
                                )
                                .child(
                                    div()
                                        .flex()
                                        .gap_2()
                                        .child(
                                            div()
                                                .w(px(60.0))
                                                .text_color(t.text_muted)
                                                .child("UInt8:"),
                                        )
                                        .child(
                                            div()
                                                .w(px(100.0))
                                                .text_right()
                                                .text_color(t.accent_success)
                                                .child(format!("{}", vals.uint8)),
                                        ),
                                )
                                // 16-bit values
                                .when_some(vals.int16, |el, v| {
                                    el.child(
                                        div()
                                            .flex()
                                            .gap_2()
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("Int16:"),
                                            )
                                            .child(
                                                div()
                                                    .w(px(100.0))
                                                    .text_right()
                                                    .text_color(t.accent_success)
                                                    .child(format!("{}", v)),
                                            ),
                                    )
                                })
                                .when_some(vals.uint16, |el, v| {
                                    el.child(
                                        div()
                                            .flex()
                                            .gap_2()
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("UInt16:"),
                                            )
                                            .child(
                                                div()
                                                    .w(px(100.0))
                                                    .text_right()
                                                    .text_color(t.accent_success)
                                                    .child(format!("{}", v)),
                                            ),
                                    )
                                })
                                // 32-bit values
                                .when_some(vals.int32, |el, v| {
                                    el.child(
                                        div()
                                            .flex()
                                            .gap_2()
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("Int32:"),
                                            )
                                            .child(
                                                div()
                                                    .w(px(100.0))
                                                    .text_right()
                                                    .text_color(t.accent_success)
                                                    .child(format!("{}", v)),
                                            ),
                                    )
                                })
                                .when_some(vals.uint32, |el, v| {
                                    el.child(
                                        div()
                                            .flex()
                                            .gap_2()
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("UInt32:"),
                                            )
                                            .child(
                                                div()
                                                    .w(px(100.0))
                                                    .text_right()
                                                    .text_color(t.accent_success)
                                                    .child(format!("{}", v)),
                                            ),
                                    )
                                }),
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
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("Int64:"),
                                            )
                                            .child(
                                                div()
                                                    .w(px(180.0))
                                                    .text_right()
                                                    .text_color(t.accent_success)
                                                    .child(format!("{}", v)),
                                            ),
                                    )
                                })
                                .when_some(vals.uint64, |el, v| {
                                    el.child(
                                        div()
                                            .flex()
                                            .gap_2()
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("UInt64:"),
                                            )
                                            .child(
                                                div()
                                                    .w(px(180.0))
                                                    .text_right()
                                                    .text_color(t.accent_success)
                                                    .child(format!("{}", v)),
                                            ),
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
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("Float32:"),
                                            )
                                            .child(
                                                div()
                                                    .w(px(180.0))
                                                    .text_right()
                                                    .text_color(t.text_warning)
                                                    .child(display),
                                            ),
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
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("Float64:"),
                                            )
                                            .child(
                                                div()
                                                    .w(px(180.0))
                                                    .text_right()
                                                    .text_color(t.text_warning)
                                                    .child(display),
                                            ),
                                    )
                                }),
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
                                        .child(
                                            div()
                                                .w(px(60.0))
                                                .text_color(t.text_muted)
                                                .child("Hex8:"),
                                        )
                                        .child(
                                            div()
                                                .text_color(t.accent_primary)
                                                .child(format!("0x{:02X}", vals.uint8)),
                                        ),
                                )
                                .when_some(vals.uint16, |el, v| {
                                    el.child(
                                        div()
                                            .flex()
                                            .gap_2()
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("Hex16:"),
                                            )
                                            .child(
                                                div()
                                                    .text_color(t.accent_primary)
                                                    .child(format!("0x{:04X}", v)),
                                            ),
                                    )
                                })
                                .when_some(vals.uint32, |el, v| {
                                    el.child(
                                        div()
                                            .flex()
                                            .gap_2()
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("Hex32:"),
                                            )
                                            .child(
                                                div()
                                                    .text_color(t.accent_primary)
                                                    .child(format!("0x{:08X}", v)),
                                            ),
                                    )
                                })
                                .when_some(vals.uint64, |el, v| {
                                    el.child(
                                        div()
                                            .flex()
                                            .gap_2()
                                            .child(
                                                div()
                                                    .w(px(60.0))
                                                    .text_color(t.text_muted)
                                                    .child("Hex64:"),
                                            )
                                            .child(
                                                div()
                                                    .text_color(t.accent_primary)
                                                    .child(format!("0x{:016X}", v)),
                                            ),
                                    )
                                }),
                        ),
                )
            })
            // No data message
            .when(self.tab().document.len() == 0, |el| {
                el.child(div().text_color(t.text_muted).child("No data available"))
            })
    }

    /// Compare tab selection dialog (modal overlay)
    pub(crate) fn render_compare_dialog(&self, cx: &mut gpui::Context<Self>) -> impl IntoElement {
        let t = &self.theme;
        let t_accent_primary = t.accent_primary;
        let t_bg_hover = t.bg_hover;
        let tabs_info: Vec<(usize, String)> = self
            .tabs
            .iter()
            .enumerate()
            .filter(|(idx, _)| *idx != self.active_tab)
            .map(|(idx, tab)| (idx, tab.display_name()))
            .collect();

        div()
            .absolute()
            .top_0()
            .left_0()
            .right_0()
            .bottom_0()
            .bg(t.modal_overlay)
            .flex()
            .items_center()
            .justify_center()
            .child(
                div()
                    .bg(t.bg_elevated)
                    .border_1()
                    .border_color(t.accent_primary)
                    .rounded_md()
                    .p_4()
                    .min_w(px(300.0))
                    .flex()
                    .flex_col()
                    .gap_3()
                    .child(
                        div()
                            .text_lg()
                            .text_color(t.accent_primary)
                            .child("Select tab to compare"),
                    )
                    .child(
                        div()
                            .text_sm()
                            .text_color(t.text_muted)
                            .child("Press 1-9 or click to select | Esc: cancel"),
                    )
                    .children(
                        tabs_info
                            .into_iter()
                            .map(move |(idx, name)| {
                                let display_num = if idx < self.active_tab { idx + 1 } else { idx };
                                div()
                                    .id(("compare-tab", idx))
                                    .flex()
                                    .gap_2()
                                    .px_3()
                                    .py_2()
                                    .rounded_md()
                                    .cursor_pointer()
                                    .bg(t_bg_hover)
                                    .hover(|h| h.bg(t_accent_primary))
                                    .on_mouse_down(
                                        gpui::MouseButton::Left,
                                        cx.listener(move |this, _event, _window, cx| {
                                            this.select_compare_tab(idx);
                                            cx.notify();
                                        }),
                                    )
                                    .child(
                                        div()
                                            .text_color(t.accent_secondary)
                                            .w(px(20.0))
                                            .child(format!("{}", display_num)),
                                    )
                                    .child(div().text_color(t.text_primary).child(name))
                            })
                            .collect::<Vec<_>>(),
                    ),
            )
    }
}

impl Render for HexEditor {
    fn render(&mut self, window: &mut Window, cx: &mut Context<Self>) -> impl IntoElement {
        // Drain pending actions from Windows native menu and dispatch on next frame
        #[cfg(target_os = "windows")]
        {
            let actions = crate::windows_menu::drain_pending_actions();
            for action in actions {
                window.on_next_frame(move |window, cx| {
                    window.dispatch_action(action, cx);
                });
            }
        }

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

        // gpui's default line_height is phi (golden ratio) * font_size.
        // All height estimates must use this to match the actual rendered layout.
        const PHI: f32 = 1.618034;

        // Main content font (configurable size)
        let font_size = px(self.settings.display.font_size);
        let ascent = window.text_system().ascent(font_id, font_size);
        // Row height = phi-based line height + mb_1 margin (4px)
        self.cached_row_height = (self.settings.display.font_size * PHI).round() + 4.0;
        // Monospace character width from em_advance (fallback to ascent * 0.6 if unavailable)
        self.cached_char_width = match window.text_system().em_advance(font_id, font_size) {
            Ok(advance) => f32::from(advance),
            Err(_) => f32::from(ascent) * 0.6, // Fallback approximation
        };

        // Address column width: dynamic based on document size
        let address_chars = ui::address_chars(self.tab().document.len());
        let address_width = self.cached_char_width * address_chars as f32;

        // text_xl (rems(1.25) = 20px at default rem 16px)
        self.cached_line_height_xl = (20.0 * PHI).round();

        // text_sm (rems(0.875) = 14px at default rem 16px)
        self.cached_line_height_sm = (14.0 * PHI).round();

        // text_xs (rems(0.75) = 12px at default rem 16px)
        self.cached_line_height_xs = (12.0 * PHI).round();

        // Update search results if search completed
        if self.update_search_results() {
            cx.notify(); // Trigger re-render
        }

        let row_count = {
            let base = ui::row_count(self.tab().document.len(), self.bytes_per_row());
            if self.tab().edit_mode == EditMode::Insert {
                // Insert mode: ensure the row exists when cursor reaches doc_len
                let max_row = self.tab().document.len() / self.bytes_per_row();
                base.max(max_row + 1)
            } else {
                base
            }
        };

        // Phase 1: Get current scroll position and handle programmatic vs user scroll
        let scroll_position = self.tab().scroll_handle.offset();

        if self.tab().scroll_logical_row.is_some() {
            if self.user_scrolled {
                // User scrolled via mouse wheel — clear the logical row anchor
                self.tab_mut().scroll_logical_row = None;
                self.user_scrolled = false;
                self.tab_mut().scroll_offset = scroll_position.y;
            } else {
                // Check for scrollbar drag: large offset change (> 2px) without mouse wheel
                let expected_offset = ui::calculate_scroll_offset(
                    self.tab().scroll_logical_row.unwrap(),
                    self.content_view_rows,
                    row_count,
                    self.row_height(),
                );
                let diff = (f32::from(scroll_position.y) - f32::from(expected_offset)).abs();
                if diff > 2.0 {
                    // Scrollbar drag or other external offset change
                    self.tab_mut().scroll_logical_row = None;
                    self.tab_mut().scroll_offset = scroll_position.y;
                } else {
                    // gpui clamping or no change — re-apply offset from logical_row
                    self.tab_mut().scroll_handle.set_offset(
                        gpui::Point::new(scroll_position.x, expected_offset),
                    );
                    self.tab_mut().scroll_offset = expected_offset;
                }
            }
        } else {
            self.user_scrolled = false;
            self.tab_mut().scroll_offset = scroll_position.y;
        }

        // Phase 2: Calculate visible range based on viewport
        // scroll_handle.bounds() returns the hex-content div's layout bounds,
        // which already represents the content area (excluding header and status bar)
        let viewport_bounds = self.tab().scroll_handle.bounds();
        let viewport_height = viewport_bounds.size.height;
        let content_height = viewport_height.max(px(20.0));

        let anchor_row = self.tab().scroll_logical_row;
        let visible_range = ui::calculate_visible_range(
            self.tab().scroll_offset,
            content_height,
            row_count,
            self.row_height(),
            anchor_row,
        );
        let render_start = visible_range.render_start;
        let render_end = visible_range.render_end;

        // Update content_view_rows from calculated visible rows
        self.content_view_rows = visible_range.visible_rows;

        // Update render cache for visible rows
        self.update_render_cache(render_start, render_end);

        // Update bitmap image cache if bitmap is visible
        if self.bitmap.visible {
            self.update_bitmap_cache();
        }

        // Phase 3: Calculate spacer heights for virtual scrolling
        // Uses capped virtual height to avoid f32 precision issues with large files
        let (top_spacer_height, bottom_spacer_height) = ui::calculate_spacer_heights(
            render_start,
            render_end,
            row_count,
            self.row_height(),
            self.tab().scroll_offset,
            visible_range.buffer_before,
        );

        // Get display title
        let title = self
            .tab()
            .document
            .file_name()
            .unwrap_or("Rust Hex Editor")
            .to_string();

        // Build render params for helper methods
        let params = RenderParams {
            font_name: font_name.clone(),
            address_width,
            address_chars,
            render_start,
            render_end,
            row_count,
            top_spacer_height,
            bottom_spacer_height,
            content_height,
            viewport_bounds,
        };

        let root = div()
            .flex()
            .flex_col()
            .bg(self.theme.bg_primary)
            .size_full()
            .p_4()
            .track_focus(&self.focus_handle)
            .on_mouse_down(
                gpui::MouseButton::Left,
                cx.listener(|_this, _event, window, cx| {
                    cx.focus_self(window);
                }),
            )
            .on_mouse_up(
                gpui::MouseButton::Left,
                cx.listener(|this, _event, _window, cx| {
                    if this.log_panel.drag_start_y.is_some() {
                        this.log_panel.drag_start_y = None;
                        this.log_panel.drag_start_height = None;
                        cx.notify();
                    }
                    if this.is_dragging {
                        this.is_dragging = false;
                        this.drag_pane = None;
                        this.bitmap.drag_start_y = None;
                        this.bitmap.drag_start_row = None;
                        cx.notify();
                    }
                }),
            )
            .on_mouse_move(
                cx.listener(|this, event: &gpui::MouseMoveEvent, _window, cx| {
                    // Cancel drag if mouse button was released outside the window
                    if this.is_dragging && !event.dragging() {
                        this.is_dragging = false;
                        this.drag_pane = None;
                        this.bitmap.drag_start_y = None;
                        this.bitmap.drag_start_row = None;
                        cx.notify();
                        return;
                    }

                    // Cancel log panel resize if mouse button was released outside
                    if this.log_panel.drag_start_y.is_some() && !event.dragging() {
                        this.log_panel.drag_start_y = None;
                        this.log_panel.drag_start_height = None;
                        cx.notify();
                        return;
                    }

                    // Handle log panel resize drag
                    if let (Some(start_y), Some(start_height)) =
                        (this.log_panel.drag_start_y, this.log_panel.drag_start_height)
                    {
                        let mouse_y: f32 = event.position.y.into();
                        // Dragging up increases height (panel grows upward)
                        let delta_y = start_y - mouse_y;
                        let new_height = (start_height + delta_y)
                            .clamp(crate::log_panel::MIN_HEIGHT, crate::log_panel::MAX_HEIGHT);
                        this.log_panel.panel_height = new_height;
                        cx.notify();
                        return;
                    }

                    // Handle bitmap viewport indicator drag at root level
                    if this.drag_pane == Some(EditPane::Bitmap) && this.is_dragging {
                        if let (Some(start_y), Some(start_row)) =
                            (this.bitmap.drag_start_y, this.bitmap.drag_start_row)
                        {
                            let mouse_y: f32 = event.position.y.into();
                            let delta_y = mouse_y - start_y;

                            // Convert pixel delta to bitmap rows
                            let bitmap_width = this.bitmap.width;
                            let bitmap_panel_width = this.bitmap.panel_width;
                            let pixel_size = ((bitmap_panel_width - 20.0) / bitmap_width as f32)
                                .max(1.0)
                                .min(4.0);
                            let delta_bitmap_rows = (delta_y / pixel_size) as isize;

                            // Convert bitmap rows to hex view rows
                            let bytes_per_row = this.bytes_per_row();
                            let delta_hex_rows =
                                delta_bitmap_rows * bitmap_width as isize / bytes_per_row as isize;

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

                    // Handle hex/ASCII drag selection at root level
                    // Root-level handler ensures events fire even when mouse moves outside hex-content div
                    if (this.drag_pane == Some(EditPane::Hex)
                        || this.drag_pane == Some(EditPane::Ascii))
                        && this.is_dragging
                    {
                        let bytes_per_row = this.bytes_per_row();
                        let doc_len = this.tab().document.len();
                        if doc_len == 0 {
                            return;
                        }
                        let scroll_offset_f32: f32 =
                            (-f32::from(this.tab().scroll_offset)).max(0.0);

                        let row_height = this.cached_row_height;
                        let char_width = this.cached_char_width;
                        // Hex byte: 2 chars + gap_1 (4px)
                        let hex_byte_width = char_width * 2.0 + 4.0;

                        // Calculate row from Y position
                        // event.position is in window coordinates
                        let mouse_y: f32 = event.position.y.into();

                        // Content area offset: outer padding + header height + content top padding
                        let outer_padding = 16.0; // p_4
                        let content_top_padding = 16.0; // pt_4
                        let content_top =
                            outer_padding + this.calculate_header_height() + content_top_padding;
                        let relative_y = mouse_y - content_top + scroll_offset_f32;

                        let row = if relative_y < 0.0 {
                            0
                        } else {
                            (relative_y / row_height) as usize
                        };

                        let row_start = row * bytes_per_row;

                        // Calculate byte in row from X position
                        let mouse_x: f32 = event.position.x.into();
                        // hex column starts after: outer_padding + address_column + gap_4
                        let addr_chars = crate::ui::address_chars(this.tab().document.len());
                        let bookmark_indicator = 12.0; // 8px dot + 4px margin, or 12px spacer
                        let address_width = bookmark_indicator + char_width * addr_chars as f32;
                        let hex_start = outer_padding + address_width + 16.0;
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
                            _ => 0,
                        };

                        let byte_in_row = byte_in_row.min(bytes_per_row - 1);
                        let new_cursor = (row_start + byte_in_row).min(doc_len.saturating_sub(1));

                        if this.tab().cursor_position != new_cursor {
                            this.tab_mut().cursor_position = new_cursor;
                            this.ensure_cursor_visible_by_row();
                            cx.notify();
                        }
                    }
                }),
            )
            // Action handlers (dispatched from menu bar and key bindings)
            .on_action(cx.listener(|this, _: &actions::Open, _window, cx| {
                this.open_file_dialog(cx);
            }))
            .on_action(cx.listener(|this, _: &actions::Save, window, cx| {
                this.save_with_confirmation(window, cx);
            }))
            .on_action(cx.listener(|this, _: &actions::SaveAs, _window, cx| {
                this.save_as_dialog(cx);
            }))
            .on_action(cx.listener(|this, _: &actions::SaveSelectionAs, _window, cx| {
                this.save_selection_as_dialog(cx);
            }))
            .on_action(cx.listener(|this, _: &actions::NewTab, _window, cx| {
                this.new_tab();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::CloseTab, _window, cx| {
                this.close_tab();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::Undo, _window, cx| {
                if let Some(offset) = this.tab_mut().document.undo() {
                    this.push_cursor_history();
                    this.move_position(offset);
                    this.log(crate::log_panel::LogLevel::Info, "Undo");
                }
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::Redo, _window, cx| {
                if let Some(offset) = this.tab_mut().document.redo() {
                    this.push_cursor_history();
                    this.move_position(offset);
                    this.log(crate::log_panel::LogLevel::Info, "Redo");
                }
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::Copy, _window, cx| {
                this.copy_selection(cx);
            }))
            .on_action(cx.listener(|this, _: &actions::CopyAsAscii, _window, cx| {
                this.copy_as_ascii(cx);
            }))
            .on_action(cx.listener(|this, _: &actions::CopyAsHexString, _window, cx| {
                this.copy_as_hex_string(cx);
            }))
            .on_action(cx.listener(|this, _: &actions::CopyAsCArray, _window, cx| {
                this.copy_as_c_array(cx);
            }))
            .on_action(cx.listener(|this, _: &actions::Paste, _window, cx| {
                this.paste_from_clipboard(cx);
            }))
            .on_action(cx.listener(|this, _: &actions::SelectAll, _window, cx| {
                if this.tab().document.len() > 0 {
                    this.tab_mut().selection_start = Some(0);
                    let end_pos = this.tab().document.len().saturating_sub(1);
                    this.tab_mut().cursor_position = end_pos;
                    this.log(crate::log_panel::LogLevel::Info, "Selected all");
                }
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::ToggleInsertMode, _window, cx| {
                this.tab_mut().edit_mode = match this.tab().edit_mode {
                    EditMode::Overwrite => EditMode::Insert,
                    EditMode::Insert => EditMode::Overwrite,
                };
                this.tab_mut().hex_nibble = HexNibble::High;
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::ToggleSearch, _window, cx| {
                let visible = !this.tab().search_visible;
                this.tab_mut().search_visible = visible;
                if !visible {
                    this.tab_mut().search_results.clear();
                    this.tab_mut().current_search_index = None;
                }
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::ToggleInspector, _window, cx| {
                this.toggle_inspector();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::ToggleBitmap, _window, cx| {
                this.toggle_bitmap();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::TogglePatternPanel, _window, cx| {
                this.toggle_pattern_panel();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::ToggleCompareMode, _window, cx| {
                if this.compare.mode {
                    this.exit_compare_mode();
                } else {
                    this.start_compare_mode();
                }
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::CycleEncoding, _window, cx| {
                this.cycle_encoding();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::ToggleLogPanel, _window, cx| {
                this.toggle_log_panel();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::ClearLog, _window, cx| {
                this.log_panel.clear();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::FindNext, _window, cx| {
                this.next_search_result();
                cx.notify();
            }))
            .on_action(cx.listener(|this, _: &actions::FindPrev, _window, cx| {
                this.prev_search_result();
                cx.notify();
            }))
            .on_key_down(cx.listener(keyboard::handle_key_event))
            .on_drop(cx.listener(|editor, paths: &ExternalPaths, _window, cx| {
                editor.handle_file_drop(paths, cx);
            }));

        // Windows: custom-rendered menu bar (DirectComposition covers Win32 HMENU)
        #[cfg(target_os = "windows")]
        let root = root.child(self.render_menu_bar(cx));

        root.child(self.render_header(&title))
            .when(self.tabs.len() > 1, |parent| {
                parent.child(self.render_tab_bar(cx))
            })
            .when(self.tab().search_visible, |parent| {
                parent.child(self.render_search_bar())
            })
            .when(self.tab().bookmark_comment_editing, |parent| {
                parent.child(self.render_bookmark_bar())
            })
            .when(!self.compare.mode, |parent| {
                parent.child(self.render_normal_mode(cx, &params))
            })
            .when(self.compare.mode, |parent| {
                parent.child(self.render_compare_mode(&params))
            })
            .child(self.render_status_bar(cx, &params.font_name))
            .map(|parent| {
                if self.log_panel.visible {
                    parent.child(self.render_log_panel(cx, &params.font_name))
                } else {
                    parent
                }
            })
            .when(self.inspector_visible, |parent| {
                parent.child(self.render_data_inspector(&params.font_name))
            })
            .when(self.compare.selection_visible, |parent| {
                parent.child(self.render_compare_dialog(cx))
            })
    }
}
