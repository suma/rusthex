//! Keyboard event handling for the hex editor
//!
//! This module centralizes all keyboard shortcuts and input handling,
//! including navigation, editing, search, and file operations.

use gpui::{Context, KeyDownEvent, Window};
use std::sync::atomic::Ordering;

use crate::{HexEditor, SearchMode, EditPane};

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
    // Check for Ctrl+T or Cmd+T (new tab)
    if event.keystroke.key == "t"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        editor.new_tab();
        cx.notify();
        return;
    }

    // Check for Ctrl+W or Cmd+W (close tab)
    if event.keystroke.key == "w"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        editor.close_tab();
        cx.notify();
        return;
    }

    // Check for Ctrl+Tab (next tab)
    if event.keystroke.key == "tab"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && !event.keystroke.modifiers.shift
    {
        editor.next_tab();
        cx.notify();
        return;
    }

    // Check for Ctrl+Shift+Tab (previous tab)
    if event.keystroke.key == "tab"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && event.keystroke.modifiers.shift
    {
        editor.prev_tab();
        cx.notify();
        return;
    }

    // Check for Ctrl+O or Cmd+O (open file)
    if event.keystroke.key == "o"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        editor.open_file_dialog(cx);
        return;
    }

    // Check for Ctrl+F or Cmd+F (toggle search)
    if event.keystroke.key == "f"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        let visible = !editor.tab().search_visible;
        editor.tab_mut().search_visible = visible;
        if !visible {
            editor.tab_mut().search_results.clear();
            editor.tab_mut().current_search_index = None;
        }
        cx.notify();
        return;
    }

    // Check for Ctrl+I or Cmd+I (toggle data inspector)
    if event.keystroke.key == "i"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        editor.toggle_inspector();
        cx.notify();
        return;
    }

    // Check for Ctrl+Shift+E or Cmd+Shift+E (cycle text encoding)
    if event.keystroke.key == "e"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && event.keystroke.modifiers.shift
    {
        editor.cycle_encoding();
        cx.notify();
        return;
    }

    // Check for Ctrl+E or Cmd+E (toggle endianness when inspector is visible)
    if event.keystroke.key == "e"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && !event.keystroke.modifiers.shift
        && editor.inspector_visible
    {
        editor.toggle_inspector_endian();
        cx.notify();
        return;
    }

    // Check for Ctrl+K or Cmd+K (compare mode)
    if event.keystroke.key == "k"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        if editor.compare_mode {
            editor.exit_compare_mode();
        } else {
            editor.start_compare_mode();
        }
        cx.notify();
        return;
    }

    // Handle number keys for compare tab selection (1-9)
    if editor.compare_selection_visible {
        if let Some(num) = event.keystroke.key.chars().next().and_then(|c| c.to_digit(10)) {
            if num >= 1 && num <= 9 {
                let index = (num as usize) - 1;
                editor.select_compare_tab(index);
                cx.notify();
                return;
            }
        }
    }

    // Check for Escape (close compare mode, tab selection, search, etc.)
    if event.keystroke.key == "escape" {
        // Priority: compare selection dialog > compare mode > search
        if editor.compare_selection_visible {
            editor.compare_selection_visible = false;
            cx.notify();
            return;
        }
        if editor.compare_mode {
            editor.exit_compare_mode();
            cx.notify();
            return;
        }
        if editor.tab().search_visible {
            if editor.tab().is_searching {
                // Cancel ongoing search
                editor.tab().search_cancel_flag.store(true, Ordering::Relaxed);
                editor.tab_mut().is_searching = false;
            }
            editor.tab_mut().search_visible = false;
            editor.tab_mut().search_results.clear();
            editor.tab_mut().current_search_index = None;
            cx.notify();
            return;
        }
    }

    // Check for F2 (next bookmark)
    if event.keystroke.key == "f2" && !event.keystroke.modifiers.shift {
        editor.next_bookmark();
        cx.notify();
        return;
    }

    // Check for Shift+F2 (previous bookmark)
    if event.keystroke.key == "f2" && event.keystroke.modifiers.shift {
        editor.prev_bookmark();
        cx.notify();
        return;
    }

    // Check for F3 (next search result)
    if event.keystroke.key == "f3" && !event.keystroke.modifiers.shift {
        editor.next_search_result();
        cx.notify();
        return;
    }

    // Check for Shift+F3 (previous search result)
    if event.keystroke.key == "f3" && event.keystroke.modifiers.shift {
        editor.prev_search_result();
        cx.notify();
        return;
    }

    // Check for Ctrl+A or Cmd+A (select all)
    if event.keystroke.key == "a"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && !event.keystroke.modifiers.shift
    {
        if editor.tab().document.len() > 0 {
            editor.tab_mut().selection_start = Some(0);
            let end_pos = editor.tab().document.len().saturating_sub(1);
            editor.tab_mut().cursor_position = end_pos;
            editor.save_message = Some("Selected all".to_string());
            cx.notify();
        }
        return;
    }

    // Check for Ctrl+Shift+S or Cmd+Shift+S (save as)
    if event.keystroke.key == "s"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && event.keystroke.modifiers.shift
    {
        editor.save_as_dialog(cx);
        return;
    }

    // Check for Ctrl+S or Cmd+S (save with confirmation)
    if event.keystroke.key == "s"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && !event.keystroke.modifiers.shift
    {
        editor.save_with_confirmation(window, cx);
        return;
    }

    // Check for Ctrl+B or Cmd+B (toggle bookmark)
    if event.keystroke.key == "b"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && !event.keystroke.modifiers.shift
    {
        editor.toggle_bookmark();
        cx.notify();
        return;
    }

    // Check for Ctrl+Shift+B or Cmd+Shift+B (clear all bookmarks)
    if event.keystroke.key == "b"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && event.keystroke.modifiers.shift
    {
        editor.clear_bookmarks();
        cx.notify();
        return;
    }

    // Check for Ctrl+Z or Cmd+Z (undo)
    if event.keystroke.key == "z"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && !event.keystroke.modifiers.shift
    {
        if editor.tab_mut().document.undo() {
            editor.save_message = Some("Undo".to_string());
            cx.notify();
        }
        return;
    }

    // Check for Ctrl+Y or Cmd+Y or Ctrl+Shift+Z (redo)
    if (event.keystroke.key == "y"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform))
        || (event.keystroke.key == "z"
            && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
            && event.keystroke.modifiers.shift)
    {
        if editor.tab_mut().document.redo() {
            editor.save_message = Some("Redo".to_string());
            cx.notify();
        }
        return;
    }

    // Default visible rows for page navigation (approximately one screen)
    const PAGE_ROWS: usize = 20;

    // Handle Ctrl+Home (go to file start)
    if event.keystroke.key == "home"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        if event.keystroke.modifiers.shift {
            if editor.tab().selection_start.is_none() {
                let pos = editor.tab().cursor_position;
                editor.tab_mut().selection_start = Some(pos);
            }
        } else {
            editor.clear_selection();
        }
        editor.move_cursor_file_start();
        cx.notify();
        return;
    }

    // Handle Ctrl+End (go to file end)
    if event.keystroke.key == "end"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        if event.keystroke.modifiers.shift {
            if editor.tab().selection_start.is_none() {
                let pos = editor.tab().cursor_position;
                editor.tab_mut().selection_start = Some(pos);
            }
        } else {
            editor.clear_selection();
        }
        editor.move_cursor_file_end();
        cx.notify();
        return;
    }

    // Handle navigation and other keys
    match event.keystroke.key.as_str() {
        "pageup" => {
            if event.keystroke.modifiers.shift {
                if editor.tab().selection_start.is_none() {
                    let pos = editor.tab().cursor_position;
                    editor.tab_mut().selection_start = Some(pos);
                }
            } else {
                editor.clear_selection();
            }
            editor.move_cursor_page_up(PAGE_ROWS);
            cx.notify();
        }
        "pagedown" => {
            if event.keystroke.modifiers.shift {
                if editor.tab().selection_start.is_none() {
                    let pos = editor.tab().cursor_position;
                    editor.tab_mut().selection_start = Some(pos);
                }
            } else {
                editor.clear_selection();
            }
            editor.move_cursor_page_down(PAGE_ROWS);
            cx.notify();
        }
        "home" => {
            if event.keystroke.modifiers.shift {
                if editor.tab().selection_start.is_none() {
                    let pos = editor.tab().cursor_position;
                    editor.tab_mut().selection_start = Some(pos);
                }
            } else {
                editor.clear_selection();
            }
            editor.move_cursor_home();
            cx.notify();
        }
        "end" => {
            if event.keystroke.modifiers.shift {
                if editor.tab().selection_start.is_none() {
                    let pos = editor.tab().cursor_position;
                    editor.tab_mut().selection_start = Some(pos);
                }
            } else {
                editor.clear_selection();
            }
            editor.move_cursor_end();
            cx.notify();
        }
        "up" => {
            if event.keystroke.modifiers.shift {
                // Start selection if not already selecting
                if editor.tab().selection_start.is_none() {
                    let pos = editor.tab().cursor_position;
                    editor.tab_mut().selection_start = Some(pos);
                }
                editor.move_cursor_up();
            } else {
                editor.clear_selection();
                editor.move_cursor_up();
            }
            cx.notify();
        }
        "down" => {
            if event.keystroke.modifiers.shift {
                if editor.tab().selection_start.is_none() {
                    let pos = editor.tab().cursor_position;
                    editor.tab_mut().selection_start = Some(pos);
                }
                editor.move_cursor_down();
            } else {
                editor.clear_selection();
                editor.move_cursor_down();
            }
            cx.notify();
        }
        "left" => {
            if event.keystroke.modifiers.shift {
                if editor.tab().selection_start.is_none() {
                    let pos = editor.tab().cursor_position;
                    editor.tab_mut().selection_start = Some(pos);
                }
                editor.move_cursor_left();
            } else {
                editor.clear_selection();
                editor.move_cursor_left();
            }
            cx.notify();
        }
        "right" => {
            if event.keystroke.modifiers.shift {
                if editor.tab().selection_start.is_none() {
                    let pos = editor.tab().cursor_position;
                    editor.tab_mut().selection_start = Some(pos);
                }
                editor.move_cursor_right();
            } else {
                editor.clear_selection();
                editor.move_cursor_right();
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
            if editor.tab().search_visible {
                editor.tab_mut().search_query.pop();
                editor.perform_search();
                if editor.tab().is_searching {
                    editor.start_search_refresh_loop(cx);
                }
                cx.notify();
            }
        }
        "enter" => {
            if editor.tab().search_visible {
                editor.next_search_result();
                cx.notify();
            }
        }
        "space" => {
            // Handle space key explicitly (gpui reports it as "space", not " ")
            if editor.tab().search_visible {
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
                if event.keystroke.modifiers.shift {
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

                if editor.tab().search_visible {
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
                    }
                }
            }
        }
    }
}
