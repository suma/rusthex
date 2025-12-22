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
    _window: &mut Window,
    cx: &mut Context<HexEditor>,
) {
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
        editor.search_visible = !editor.search_visible;
        if !editor.search_visible {
            editor.search_results.clear();
            editor.current_search_index = None;
        }
        cx.notify();
        return;
    }

    // Check for Escape (close search or cancel ongoing search)
    if event.keystroke.key == "escape" && editor.search_visible {
        if editor.is_searching {
            // Cancel ongoing search
            editor.search_cancel_flag.store(true, Ordering::Relaxed);
            editor.is_searching = false;
        }
        editor.search_visible = false;
        editor.search_results.clear();
        editor.current_search_index = None;
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
        if editor.document.len() > 0 {
            editor.selection_start = Some(0);
            editor.cursor_position = editor.document.len().saturating_sub(1);
            editor.save_message = Some("Selected all".to_string());
            cx.notify();
        }
        return;
    }

    // Check for Ctrl+S or Cmd+S (save)
    if event.keystroke.key == "s"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
    {
        match editor.save_file() {
            Ok(_) => {
                eprintln!("File saved successfully");
                cx.notify();
            }
            Err(e) => {
                eprintln!("Failed to save file: {}", e);
                editor.save_message = Some(format!("Error: {}", e));
                cx.notify();
            }
        }
        return;
    }

    // Check for Ctrl+Z or Cmd+Z (undo)
    if event.keystroke.key == "z"
        && (event.keystroke.modifiers.control || event.keystroke.modifiers.platform)
        && !event.keystroke.modifiers.shift
    {
        if editor.document.undo() {
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
        if editor.document.redo() {
            editor.save_message = Some("Redo".to_string());
            cx.notify();
        }
        return;
    }

    // Handle navigation and other keys
    match event.keystroke.key.as_str() {
        "up" => {
            if event.keystroke.modifiers.shift {
                // Start selection if not already selecting
                if editor.selection_start.is_none() {
                    editor.selection_start = Some(editor.cursor_position);
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
                if editor.selection_start.is_none() {
                    editor.selection_start = Some(editor.cursor_position);
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
                if editor.selection_start.is_none() {
                    editor.selection_start = Some(editor.cursor_position);
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
                if editor.selection_start.is_none() {
                    editor.selection_start = Some(editor.cursor_position);
                }
                editor.move_cursor_right();
            } else {
                editor.clear_selection();
                editor.move_cursor_right();
            }
            cx.notify();
        }
        "tab" => {
            if editor.search_visible {
                // Toggle search mode
                editor.search_mode = match editor.search_mode {
                    SearchMode::Ascii => SearchMode::Hex,
                    SearchMode::Hex => SearchMode::Ascii,
                };
                editor.perform_search();
            } else {
                editor.toggle_pane();
            }
            cx.notify();
        }
        "backspace" => {
            if editor.search_visible {
                editor.search_query.pop();
                editor.perform_search();
                cx.notify();
            }
        }
        "enter" => {
            if editor.search_visible {
                editor.next_search_result();
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

                if editor.search_visible {
                    // Add character to search query
                    if (c >= ' ' && c <= '~') || c.is_whitespace() {
                        editor.search_query.push(c);
                        editor.perform_search();
                        cx.notify();
                    }
                } else {
                    // Normal editing
                    match editor.edit_pane {
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
