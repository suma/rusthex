//! Keyboard event handling for the hex editor
//!
//! This module centralizes all keyboard shortcuts and input handling,
//! including navigation, editing, search, and file operations.

use gpui::{Context, KeyDownEvent, Window};
use std::sync::atomic::Ordering;

use crate::{EditPane, HexEditor, SearchMode};

/// Check if Ctrl (Windows/Linux) or Cmd (macOS) modifier is held
fn is_cmd(event: &KeyDownEvent) -> bool {
    event.keystroke.modifiers.control || event.keystroke.modifiers.platform
}

/// Begin or continue selection if Shift is held, otherwise clear selection
fn update_selection(editor: &mut HexEditor, shift: bool) {
    if shift {
        if editor.tab().selection_start.is_none() {
            let pos = editor.tab().cursor_position;
            editor.tab_mut().selection_start = Some(pos);
        }
    } else {
        editor.clear_selection();
    }
}

/// Handle all keyboard events for the hex editor
///
/// This function dispatches keyboard events to appropriate handlers based on
/// the keystroke and current editor state (search mode, edit pane, etc.)
pub fn handle_key_event(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    window: &mut Window,
    cx: &mut Context<HexEditor>,
) {
    if handle_command_shortcuts(editor, event, window, cx) {
        return;
    }
    if handle_special_keys(editor, event, cx) {
        return;
    }
    handle_navigation_and_input(editor, event, cx);
}

/// Handle Ctrl/Cmd+key shortcuts
///
/// Returns true if the event was handled.
fn handle_command_shortcuts(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    window: &mut Window,
    cx: &mut Context<HexEditor>,
) -> bool {
    if !is_cmd(event) {
        return false;
    }
    let shift = event.keystroke.modifiers.shift;

    match (event.keystroke.key.as_str(), shift) {
        ("t", _) => editor.new_tab(),
        ("w", _) => { editor.close_tab(); }
        ("tab", false) => editor.next_tab(),
        ("tab", true) => editor.prev_tab(),
        ("o", _) => {
            editor.open_file_dialog(cx);
            return true;
        }
        ("s", true) => {
            editor.save_as_dialog(cx);
            return true;
        }
        ("s", false) => {
            editor.save_with_confirmation(window, cx);
            return true;
        }
        ("f", _) => {
            let visible = !editor.tab().search_visible;
            editor.tab_mut().search_visible = visible;
            if !visible {
                editor.tab_mut().search_results.clear();
                editor.tab_mut().current_search_index = None;
            }
        }
        ("i", _) => editor.toggle_inspector(),
        ("e", true) => editor.cycle_encoding(),
        ("e", false) if editor.inspector_visible => editor.toggle_inspector_endian(),
        ("k", _) => {
            if editor.compare_mode {
                editor.exit_compare_mode();
            } else {
                editor.start_compare_mode();
            }
        }
        ("a", false) => {
            if editor.tab().document.len() > 0 {
                editor.tab_mut().selection_start = Some(0);
                let end_pos = editor.tab().document.len().saturating_sub(1);
                editor.tab_mut().cursor_position = end_pos;
                editor.save_message = Some("Selected all".to_string());
            }
        }
        ("b", false) => editor.toggle_bookmark(),
        ("b", true) => editor.edit_bookmark_comment(),
        ("m", false) => editor.toggle_bitmap(),
        ("z", false) => {
            if editor.tab_mut().document.undo() {
                editor.save_message = Some("Undo".to_string());
            }
        }
        ("z", true) | ("y", _) => {
            if editor.tab_mut().document.redo() {
                editor.save_message = Some("Redo".to_string());
            }
        }
        ("home", _) => {
            update_selection(editor, shift);
            editor.move_cursor_file_start();
        }
        ("end", _) => {
            update_selection(editor, shift);
            editor.move_cursor_file_end();
        }
        _ => return false,
    }

    cx.notify();
    true
}

/// Handle special keys: Escape, F2, F3, compare number selection, bitmap controls
///
/// Returns true if the event was handled.
fn handle_special_keys(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    cx: &mut Context<HexEditor>,
) -> bool {
    let shift = event.keystroke.modifiers.shift;

    match event.keystroke.key.as_str() {
        "escape" => {
            // Priority: bookmark comment editing > compare selection dialog > compare mode > search
            if editor.tab().bookmark_comment_editing {
                editor.cancel_bookmark_comment();
            } else if editor.compare_selection_visible {
                editor.compare_selection_visible = false;
            } else if editor.compare_mode {
                editor.exit_compare_mode();
            } else if editor.tab().search_visible {
                if editor.tab().is_searching {
                    editor.tab().search_cancel_flag.store(true, Ordering::Relaxed);
                    editor.tab_mut().is_searching = false;
                }
                editor.tab_mut().search_visible = false;
                editor.tab_mut().search_results.clear();
                editor.tab_mut().current_search_index = None;
            } else {
                return false;
            }
        }
        "f2" if !shift => editor.next_bookmark(),
        "f2" if shift => editor.prev_bookmark(),
        "f3" if !shift => editor.next_search_result(),
        "f3" if shift => editor.prev_search_result(),
        key if editor.compare_selection_visible => {
            if let Some(num) = key.chars().next().and_then(|c| c.to_digit(10)) {
                if num >= 1 && num <= 9 {
                    let index = (num as usize) - 1;
                    editor.select_compare_tab(index);
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
        key if editor.bitmap_visible => {
            match key {
                "c" if !is_cmd(event) => editor.cycle_bitmap_color_mode(),
                "=" | "+" => editor.increase_bitmap_width(),
                "-" => editor.decrease_bitmap_width(),
                _ => return false,
            }
        }
        _ => return false,
    }

    cx.notify();
    true
}

/// Default visible rows for page navigation (approximately one screen)
const PAGE_ROWS: usize = 20;

/// Handle navigation keys and text input
fn handle_navigation_and_input(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    cx: &mut Context<HexEditor>,
) {
    let key = event.keystroke.key.as_str();
    let shift = event.keystroke.modifiers.shift;

    match key {
        "up" | "down" | "left" | "right" | "pageup" | "pagedown" | "home" | "end" => {
            update_selection(editor, shift);
            match key {
                "up" => editor.move_cursor_up(),
                "down" => editor.move_cursor_down(),
                "left" => editor.move_cursor_left(),
                "right" => editor.move_cursor_right(),
                "pageup" => editor.move_cursor_page_up(PAGE_ROWS),
                "pagedown" => editor.move_cursor_page_down(PAGE_ROWS),
                "home" => editor.move_cursor_home(),
                "end" => editor.move_cursor_end(),
                _ => unreachable!(),
            }
            cx.notify();
        }
        "tab" => {
            if editor.tab().search_visible {
                // Toggle search mode
                let new_mode = match editor.tab().search_mode {
                    SearchMode::Ascii => SearchMode::Hex,
                    SearchMode::Hex => SearchMode::Ascii,
                };
                editor.tab_mut().search_mode = new_mode;
                editor.perform_search();
                if editor.tab().is_searching {
                    editor.start_search_refresh_loop(cx);
                }
            } else {
                editor.toggle_pane();
            }
            cx.notify();
        }
        "backspace" => {
            if editor.tab().bookmark_comment_editing {
                editor.tab_mut().bookmark_comment_text.pop();
                cx.notify();
            } else if editor.tab().search_visible {
                editor.tab_mut().search_query.pop();
                editor.perform_search();
                if editor.tab().is_searching {
                    editor.start_search_refresh_loop(cx);
                }
                cx.notify();
            }
        }
        "enter" => {
            if editor.tab().bookmark_comment_editing {
                editor.confirm_bookmark_comment();
                cx.notify();
            } else if editor.tab().search_visible {
                editor.next_search_result();
                cx.notify();
            }
        }
        "space" => {
            // Handle space key explicitly (gpui reports it as "space", not " ")
            if editor.tab().bookmark_comment_editing {
                editor.tab_mut().bookmark_comment_text.push(' ');
                cx.notify();
            } else if editor.tab().search_visible {
                editor.tab_mut().search_query.push(' ');
                editor.perform_search();
                if editor.tab().is_searching {
                    editor.start_search_refresh_loop(cx);
                }
                cx.notify();
            }
        }
        key => {
            // Handle character input
            if key.len() == 1 {
                let mut c = key.chars().next().unwrap();

                // Handle Shift modifier for uppercase and symbols
                if shift {
                    c = match c {
                        'a'..='z' => c.to_ascii_uppercase(),
                        // Handle shifted number row symbols
                        '1' => '!',
                        '2' => '@',
                        '3' => '#',
                        '4' => '$',
                        '5' => '%',
                        '6' => '^',
                        '7' => '&',
                        '8' => '*',
                        '9' => '(',
                        '0' => ')',
                        '-' => '_',
                        '=' => '+',
                        '[' => '{',
                        ']' => '}',
                        '\\' => '|',
                        ';' => ':',
                        '\'' => '"',
                        ',' => '<',
                        '.' => '>',
                        '/' => '?',
                        '`' => '~',
                        _ => c, // Keep other characters as-is
                    };
                }

                if editor.tab().bookmark_comment_editing {
                    // Add character to bookmark comment
                    if (c >= ' ' && c <= '~') || c.is_whitespace() {
                        editor.tab_mut().bookmark_comment_text.push(c);
                        cx.notify();
                    }
                } else if editor.tab().search_visible {
                    // Add character to search query
                    if (c >= ' ' && c <= '~') || c.is_whitespace() {
                        editor.tab_mut().search_query.push(c);
                        editor.perform_search();
                        if editor.tab().is_searching {
                            editor.start_search_refresh_loop(cx);
                        }
                        cx.notify();
                    }
                } else {
                    // Normal editing
                    match editor.tab().edit_pane {
                        EditPane::Hex => {
                            if c.is_ascii_hexdigit() {
                                editor.input_hex(c);
                                cx.notify();
                            }
                        }
                        EditPane::Ascii => {
                            if c >= ' ' && c <= '~' {
                                editor.input_ascii(c);
                                cx.notify();
                            }
                        }
                        EditPane::Bitmap => {
                            // Bitmap mode doesn't accept character input
                        }
                    }
                }
            }
        }
    }
}
