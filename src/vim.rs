//! Vim-like keybinding mode for the hex editor
//!
//! When `vim_mode = true` in config.toml, this module provides Normal/Insert/Visual/Command
//! modes with standard Vim keybindings. Insert mode falls through to the existing input
//! handling, so all existing editing behavior is preserved.

use gpui::{Context, KeyDownEvent, Window};

use crate::ui::EditPane;
use crate::{EditMode, HexEditor, HexNibble};

/// Vim mode states
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VimMode {
    Normal,
    Insert,
    Visual,
    Command,
}

/// Pending operator for operator-motion combos (d, y, c)
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VimOperator {
    Delete,
    Yank,
    Change,
}

/// Vim state machine, stored per-tab
pub struct VimState {
    pub mode: VimMode,
    pub count: Option<usize>,
    pub pending_operator: Option<VimOperator>,
    pub pending_key: Option<char>,
    pub command_buffer: String,
    pub yank_buffer: Vec<u8>,
}

impl VimState {
    pub fn new() -> Self {
        Self {
            mode: VimMode::Normal,
            count: None,
            pending_operator: None,
            pending_key: None,
            command_buffer: String::new(),
            yank_buffer: Vec::new(),
        }
    }

    /// Enter a new mode, resetting pending state
    pub fn enter_mode(&mut self, mode: VimMode) {
        self.mode = mode;
        self.count = None;
        self.pending_operator = None;
        self.pending_key = None;
        if mode != VimMode::Command {
            self.command_buffer.clear();
        }
    }

    /// Accumulate a digit into the count prefix
    pub fn accumulate_count(&mut self, digit: u8) {
        let current = self.count.unwrap_or(0);
        self.count = Some(current * 10 + digit as usize);
    }

    /// Take the count and reset it (default 1)
    pub fn take_count(&mut self) -> usize {
        self.count.take().unwrap_or(1)
    }

    /// Reset pending operator and key
    pub fn reset_pending(&mut self) {
        self.count = None;
        self.pending_operator = None;
        self.pending_key = None;
    }
}

impl Default for VimState {
    fn default() -> Self {
        Self::new()
    }
}

/// Main Vim key dispatcher. Returns true if the key was handled.
pub fn handle_vim_key(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    window: &mut Window,
    cx: &mut Context<HexEditor>,
) -> bool {
    // Skip Vim handling when UI input fields are active
    if editor.tab().search_visible
        || editor.tab().goto_address_visible
        || editor.tab().bookmark_comment_editing
        || editor.pattern_dropdown_open
        || editor.compare.selection_visible
        || editor.about_visible
    {
        return false;
    }

    let mode = editor.tab().vim_state.mode;
    let handled = match mode {
        VimMode::Normal => handle_normal_mode(editor, event, cx),
        VimMode::Insert => handle_insert_mode(editor, event, cx),
        VimMode::Visual => handle_visual_mode(editor, event, cx),
        VimMode::Command => handle_command_mode(editor, event, window, cx),
    };

    if handled {
        cx.notify();
    }
    handled
}

// ─── Normal Mode ───────────────────────────────────────────────────────────────

fn handle_normal_mode(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    _cx: &mut Context<HexEditor>,
) -> bool {
    let key = event.keystroke.key.as_str();
    let ctrl = event.keystroke.modifiers.control;
    let shift = event.keystroke.modifiers.shift;

    // Ctrl+f / Ctrl+b for page navigation
    if ctrl {
        match key {
            "f" => {
                let count = editor.tab_mut().vim_state.take_count();
                for _ in 0..count {
                    editor.move_cursor_page_down(crate::keyboard::PAGE_ROWS);
                }
                return true;
            }
            "b" => {
                let count = editor.tab_mut().vim_state.take_count();
                for _ in 0..count {
                    editor.move_cursor_page_up(crate::keyboard::PAGE_ROWS);
                }
                return true;
            }
            "r" => {
                // Ctrl+r = Redo
                if let Some(offset) = editor.tab_mut().document.redo() {
                    editor.push_cursor_history();
                    editor.move_position(offset);
                    editor.log(crate::log_panel::LogLevel::Info, "Redo");
                }
                return true;
            }
            _ => return false,
        }
    }

    // Check for pending 'g' key (for gg)
    if editor.tab().vim_state.pending_key == Some('g') {
        editor.tab_mut().vim_state.pending_key = None;
        if key == "g" {
            // gg: go to file start
            editor.push_cursor_history();
            editor.clear_selection();
            editor.move_cursor_file_start();
            editor.tab_mut().vim_state.reset_pending();
            return true;
        }
        // Other key after g: reset and fall through
        editor.tab_mut().vim_state.reset_pending();
    }

    // Check for pending 'r' key (replace single byte)
    if editor.tab().vim_state.pending_key == Some('r') {
        editor.tab_mut().vim_state.pending_key = None;
        return handle_replace_char(editor, event);
    }

    // Check for pending operator (d, y, c) waiting for motion
    if let Some(op) = editor.tab().vim_state.pending_operator {
        return handle_operator_motion(editor, event, op);
    }

    // Handle shifted keys
    if shift {
        match key {
            "g" => {
                // G: go to file end, or {count}G for address jump
                let count = editor.tab_mut().vim_state.count.take();
                editor.push_cursor_history();
                editor.clear_selection();
                if let Some(addr) = count {
                    // {count}G: jump to byte offset
                    editor.move_position(addr);
                } else {
                    editor.move_cursor_file_end();
                }
                editor.tab_mut().vim_state.reset_pending();
                return true;
            }
            "a" => {
                // A: go to end of row, then insert mode
                editor.move_cursor_end();
                editor.tab_mut().edit_mode = EditMode::Insert;
                editor.tab_mut().vim_state.enter_mode(VimMode::Insert);
                return true;
            }
            "i" => {
                // I: go to start of row, then insert mode
                editor.move_cursor_home();
                editor.tab_mut().edit_mode = EditMode::Insert;
                editor.tab_mut().vim_state.enter_mode(VimMode::Insert);
                return true;
            }
            "n" => {
                // N: previous search result
                editor.prev_search_result();
                return true;
            }
            "p" => {
                // P: paste before cursor
                paste_bytes(editor, false);
                return true;
            }
            "4" => {
                // $: end of row
                let count = editor.tab_mut().vim_state.take_count();
                for _ in 0..count {
                    editor.move_cursor_end();
                }
                return true;
            }
            _ => {}
        }
    }

    match key {
        // Movement
        "h" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_left();
            }
            true
        }
        "l" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_right();
            }
            true
        }
        "j" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_down();
            }
            true
        }
        "k" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_up();
            }
            true
        }
        "0" => {
            if editor.tab().vim_state.count.is_some() {
                // Part of a number prefix
                editor.tab_mut().vim_state.accumulate_count(0);
                true
            } else {
                // 0: go to start of row
                editor.move_cursor_home();
                true
            }
        }
        "w" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_word_forward();
            }
            true
        }
        "b" if !shift => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_word_backward();
            }
            true
        }
        "g" if !shift => {
            editor.tab_mut().vim_state.pending_key = Some('g');
            true
        }

        // Digits 1-9 for count prefix
        "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" if !shift => {
            let digit = key.parse::<u8>().unwrap();
            editor.tab_mut().vim_state.accumulate_count(digit);
            true
        }

        // Mode switching
        "i" if !shift => {
            editor.tab_mut().edit_mode = EditMode::Insert;
            editor.tab_mut().vim_state.enter_mode(VimMode::Insert);
            true
        }
        "a" if !shift => {
            // a: move right one byte, then insert
            editor.move_cursor_right();
            editor.tab_mut().edit_mode = EditMode::Insert;
            editor.tab_mut().vim_state.enter_mode(VimMode::Insert);
            true
        }
        "v" if !shift => {
            let pos = editor.tab().cursor_position;
            editor.tab_mut().selection_start = Some(pos);
            editor.tab_mut().vim_state.enter_mode(VimMode::Visual);
            true
        }

        // Operators
        "x" if !shift => {
            // x: delete byte at cursor
            delete_at_cursor(editor, 1);
            true
        }
        "r" if !shift => {
            // r: replace — wait for next key
            editor.tab_mut().vim_state.pending_key = Some('r');
            true
        }
        "d" if !shift => {
            editor.tab_mut().vim_state.pending_operator = Some(VimOperator::Delete);
            true
        }
        "y" if !shift => {
            editor.tab_mut().vim_state.pending_operator = Some(VimOperator::Yank);
            true
        }
        "c" if !shift => {
            editor.tab_mut().vim_state.pending_operator = Some(VimOperator::Change);
            true
        }
        "p" if !shift => {
            paste_bytes(editor, true);
            true
        }
        "u" if !shift => {
            // Undo
            if let Some(offset) = editor.tab_mut().document.undo() {
                editor.push_cursor_history();
                editor.move_position(offset);
                editor.log(crate::log_panel::LogLevel::Info, "Undo");
            }
            true
        }

        // Search navigation
        "n" if !shift => {
            editor.next_search_result();
            true
        }

        // Search
        "/" => {
            // Open search bar
            editor.tab_mut().search_visible = true;
            editor.tab_mut().goto_address_visible = false;
            editor.tab_mut().bookmark_comment_editing = false;
            true
        }

        // Command mode
        ";" if shift => {
            // : (colon) — enter command mode
            editor.tab_mut().vim_state.command_buffer.clear();
            editor.tab_mut().vim_state.enter_mode(VimMode::Command);
            true
        }

        _ => false,
    }
}

// ─── Insert Mode ───────────────────────────────────────────────────────────────

fn handle_insert_mode(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    _cx: &mut Context<HexEditor>,
) -> bool {
    let key = event.keystroke.key.as_str();

    if key == "escape" {
        // Return to Normal mode
        editor.tab_mut().edit_mode = EditMode::Overwrite;
        editor.tab_mut().vim_state.enter_mode(VimMode::Normal);
        // Move cursor left (Vim convention: cursor stays on last edited char)
        if editor.tab().cursor_position > 0 {
            editor.move_cursor_left();
        }
        return true;
    }

    // All other keys fall through to existing handle_special_keys / handle_navigation_and_input
    false
}

// ─── Visual Mode ───────────────────────────────────────────────────────────────

fn handle_visual_mode(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    _cx: &mut Context<HexEditor>,
) -> bool {
    let key = event.keystroke.key.as_str();
    let shift = event.keystroke.modifiers.shift;
    let ctrl = event.keystroke.modifiers.control;

    match key {
        "escape" => {
            editor.clear_selection();
            editor.tab_mut().vim_state.enter_mode(VimMode::Normal);
            true
        }
        // Movement keys (selection_start stays fixed, cursor_position moves)
        "h" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_left();
            }
            true
        }
        "l" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_right();
            }
            true
        }
        "j" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_down();
            }
            true
        }
        "k" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_up();
            }
            true
        }
        "0" => {
            if editor.tab().vim_state.count.is_some() {
                editor.tab_mut().vim_state.accumulate_count(0);
            } else {
                editor.move_cursor_home();
            }
            true
        }
        "w" => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_word_forward();
            }
            true
        }
        "b" if !shift => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_word_backward();
            }
            true
        }
        "g" if !shift => {
            if editor.tab().vim_state.pending_key == Some('g') {
                editor.tab_mut().vim_state.pending_key = None;
                editor.push_cursor_history();
                editor.move_cursor_file_start();
                editor.tab_mut().vim_state.reset_pending();
            } else {
                editor.tab_mut().vim_state.pending_key = Some('g');
            }
            true
        }
        "g" if shift => {
            // G: file end
            editor.push_cursor_history();
            editor.move_cursor_file_end();
            editor.tab_mut().vim_state.reset_pending();
            true
        }
        "f" if ctrl => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_page_down(crate::keyboard::PAGE_ROWS);
            }
            true
        }
        "b" if ctrl => {
            let count = editor.tab_mut().vim_state.take_count();
            for _ in 0..count {
                editor.move_cursor_page_up(crate::keyboard::PAGE_ROWS);
            }
            true
        }

        // Digits for count
        "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9" if !shift => {
            let digit = key.parse::<u8>().unwrap();
            editor.tab_mut().vim_state.accumulate_count(digit);
            true
        }

        // $ end of row
        "4" if shift => {
            editor.move_cursor_end();
            true
        }

        // Operations on selection
        "y" if !shift => {
            yank_selection(editor);
            editor.clear_selection();
            editor.tab_mut().vim_state.enter_mode(VimMode::Normal);
            true
        }
        "d" | "x" if !shift => {
            yank_selection(editor);
            delete_selection(editor);
            editor.tab_mut().vim_state.enter_mode(VimMode::Normal);
            true
        }

        _ => false,
    }
}

// ─── Command Mode ──────────────────────────────────────────────────────────────

fn handle_command_mode(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    window: &mut Window,
    cx: &mut Context<HexEditor>,
) -> bool {
    let key = event.keystroke.key.as_str();
    let shift = event.keystroke.modifiers.shift;

    match key {
        "escape" => {
            editor.tab_mut().vim_state.enter_mode(VimMode::Normal);
            true
        }
        "enter" => {
            let cmd = editor.tab().vim_state.command_buffer.clone();
            editor.tab_mut().vim_state.enter_mode(VimMode::Normal);
            execute_command(editor, &cmd, window, cx);
            true
        }
        "backspace" => {
            if editor.tab().vim_state.command_buffer.is_empty() {
                editor.tab_mut().vim_state.enter_mode(VimMode::Normal);
            } else {
                editor.tab_mut().vim_state.command_buffer.pop();
            }
            true
        }
        "space" => {
            editor.tab_mut().vim_state.command_buffer.push(' ');
            true
        }
        _ if key.len() == 1 => {
            let mut c = key.chars().next().unwrap();
            if shift {
                c = match c {
                    'a'..='z' => c.to_ascii_uppercase(),
                    '1' => '!',
                    '0' => ')',
                    _ => c,
                };
            }
            editor.tab_mut().vim_state.command_buffer.push(c);
            true
        }
        _ => true, // Consume all keys in command mode
    }
}

fn execute_command(
    editor: &mut HexEditor,
    cmd: &str,
    window: &mut Window,
    cx: &mut Context<HexEditor>,
) {
    let cmd = cmd.trim();

    match cmd {
        "w" => {
            editor.save_with_confirmation(window, cx);
        }
        "q" => {
            if editor.tab().document.has_unsaved_changes() && !editor.force_close {
                editor.log(
                    crate::log_panel::LogLevel::Warning,
                    "Unsaved changes. Use :q! to force quit.",
                );
            } else {
                cx.quit();
            }
        }
        "wq" | "x" => {
            editor.save_with_confirmation(window, cx);
            cx.quit();
        }
        "q!" => {
            editor.force_close = true;
            cx.quit();
        }
        _ => {
            // Try parsing as address jump
            if let Some(addr) = parse_address(cmd) {
                editor.push_cursor_history();
                editor.clear_selection();
                editor.move_position(addr);
            } else {
                editor.log(
                    crate::log_panel::LogLevel::Warning,
                    &format!("Unknown command: :{}", cmd),
                );
            }
        }
    }
}

/// Parse an address from command input.
/// - `0x...` or `0X...` → hex byte offset
/// - decimal number → row number × bytes_per_row (treated as line number)
fn parse_address(cmd: &str) -> Option<usize> {
    let cmd = cmd.trim();
    if cmd.starts_with("0x") || cmd.starts_with("0X") {
        usize::from_str_radix(&cmd[2..], 16).ok()
    } else {
        cmd.parse::<usize>().ok()
    }
}

// ─── Operator-Motion handling ──────────────────────────────────────────────────

fn handle_operator_motion(
    editor: &mut HexEditor,
    event: &KeyDownEvent,
    op: VimOperator,
) -> bool {
    let key = event.keystroke.key.as_str();
    let shift = event.keystroke.modifiers.shift;

    // dd / yy / cc — line-wise operation
    let op_char = match op {
        VimOperator::Delete => "d",
        VimOperator::Yank => "y",
        VimOperator::Change => "c",
    };

    if key == op_char && !shift {
        // Line-wise: operate on bytes_per_row bytes from start of current row
        let bpr = editor.bytes_per_row();
        let count = editor.tab_mut().vim_state.take_count();
        let row_start = (editor.tab().cursor_position / bpr) * bpr;
        let byte_count = (count * bpr).min(editor.tab().document.len().saturating_sub(row_start));
        let range_start = row_start;
        let range_end = row_start + byte_count;

        // Yank the range
        let bytes = editor
            .tab()
            .document
            .get_slice(range_start..range_end)
            .unwrap_or_default();
        editor.tab_mut().vim_state.yank_buffer = bytes;

        match op {
            VimOperator::Delete => {
                delete_range(editor, range_start, byte_count);
            }
            VimOperator::Yank => {
                // Just yank, no delete
                editor.log(
                    crate::log_panel::LogLevel::Info,
                    &format!("Yanked {} bytes", byte_count),
                );
            }
            VimOperator::Change => {
                delete_range(editor, range_start, byte_count);
                editor.tab_mut().edit_mode = EditMode::Insert;
                editor.tab_mut().vim_state.enter_mode(VimMode::Insert);
                return true;
            }
        }
        editor.tab_mut().vim_state.reset_pending();
        return true;
    }

    // operator + motion: compute motion range, then apply
    let cursor_before = editor.tab().cursor_position;
    let motion_applied = apply_motion(editor, key, shift);

    if motion_applied {
        let cursor_after = editor.tab().cursor_position;
        let (start, end) = if cursor_before <= cursor_after {
            (cursor_before, cursor_after)
        } else {
            (cursor_after, cursor_before)
        };
        let byte_count = end - start + 1;

        // Yank
        let bytes = editor
            .tab()
            .document
            .get_slice(start..end + 1)
            .unwrap_or_default();
        editor.tab_mut().vim_state.yank_buffer = bytes;

        match op {
            VimOperator::Delete => {
                // Move cursor back to start of range
                editor.move_position(start);
                delete_range(editor, start, byte_count);
            }
            VimOperator::Yank => {
                // Restore cursor
                editor.move_position(cursor_before);
                editor.log(
                    crate::log_panel::LogLevel::Info,
                    &format!("Yanked {} bytes", byte_count),
                );
            }
            VimOperator::Change => {
                editor.move_position(start);
                delete_range(editor, start, byte_count);
                editor.tab_mut().edit_mode = EditMode::Insert;
                editor.tab_mut().vim_state.enter_mode(VimMode::Insert);
                return true;
            }
        }
        editor.tab_mut().vim_state.reset_pending();
        true
    } else {
        // Unknown motion, cancel operator
        editor.tab_mut().vim_state.reset_pending();
        false
    }
}

/// Apply a motion key and return whether it was a valid motion
fn apply_motion(editor: &mut HexEditor, key: &str, shift: bool) -> bool {
    match key {
        "h" => {
            editor.move_cursor_left();
            true
        }
        "l" => {
            editor.move_cursor_right();
            true
        }
        "j" => {
            editor.move_cursor_down();
            true
        }
        "k" => {
            editor.move_cursor_up();
            true
        }
        "w" => {
            editor.move_cursor_word_forward();
            true
        }
        "b" if !shift => {
            editor.move_cursor_word_backward();
            true
        }
        "0" => {
            editor.move_cursor_home();
            true
        }
        "4" if shift => {
            // $
            editor.move_cursor_end();
            true
        }
        "g" if shift => {
            // G: file end
            editor.move_cursor_file_end();
            true
        }
        _ => false,
    }
}

// ─── Replace char ──────────────────────────────────────────────────────────────

fn handle_replace_char(editor: &mut HexEditor, event: &KeyDownEvent) -> bool {
    let key = event.keystroke.key.as_str();
    let shift = event.keystroke.modifiers.shift;

    if key == "escape" {
        editor.tab_mut().vim_state.reset_pending();
        return true;
    }

    if key.len() != 1 {
        editor.tab_mut().vim_state.reset_pending();
        return true;
    }

    let mut c = key.chars().next().unwrap();
    if shift {
        c = match c {
            'a'..='z' => c.to_ascii_uppercase(),
            _ => c,
        };
    }

    let pos = editor.tab().cursor_position;
    if pos >= editor.tab().document.len() {
        editor.tab_mut().vim_state.reset_pending();
        return true;
    }

    match editor.tab().edit_pane {
        EditPane::Hex => {
            // For hex pane, need two hex digits for replacement
            // For simplicity, treat the input as a single hex digit replacement
            // and replace with the digit repeated: e.g. 'a' → 0xAA
            // Actually, let's use the character's byte value for ASCII pane behavior
            // and for hex pane, only accept hex digits
            if c.is_ascii_hexdigit() {
                let digit = c.to_digit(16).unwrap() as u8;
                let new_byte = (digit << 4) | digit; // e.g., 'a' → 0xAA
                let _ = editor.tab_mut().document.set_byte(pos, new_byte);
                editor.invalidate_render_cache();
            }
        }
        EditPane::Ascii | EditPane::Bitmap => {
            if c >= ' ' && c <= '~' {
                let _ = editor.tab_mut().document.set_byte(pos, c as u8);
                editor.invalidate_render_cache();
            }
        }
    }

    editor.tab_mut().vim_state.reset_pending();
    true
}

// ─── Helper functions ──────────────────────────────────────────────────────────

fn delete_at_cursor(editor: &mut HexEditor, count: usize) {
    let pos = editor.tab().cursor_position;
    let doc_len = editor.tab().document.len();
    if pos >= doc_len || doc_len == 0 {
        return;
    }
    let actual_count = count.min(doc_len - pos);

    // Yank the deleted bytes
    let bytes = editor
        .tab()
        .document
        .get_slice(pos..pos + actual_count)
        .unwrap_or_default();
    editor.tab_mut().vim_state.yank_buffer = bytes;

    if let Err(e) = editor.tab_mut().document.delete_bytes(pos, actual_count) {
        eprintln!("Failed to delete bytes: {}", e);
        return;
    }
    editor.invalidate_render_cache();
    editor.tab_mut().hex_nibble = HexNibble::High;
    // Adjust cursor if it's now past end
    let new_len = editor.tab().document.len();
    if pos >= new_len && new_len > 0 {
        editor.move_position(new_len - 1);
    }
}

fn delete_range(editor: &mut HexEditor, start: usize, count: usize) {
    if count == 0 {
        return;
    }
    if let Err(e) = editor.tab_mut().document.delete_bytes(start, count) {
        eprintln!("Failed to delete bytes: {}", e);
        return;
    }
    editor.invalidate_render_cache();
    editor.tab_mut().hex_nibble = HexNibble::High;
    // Adjust cursor
    let new_len = editor.tab().document.len();
    if start >= new_len && new_len > 0 {
        editor.move_position(new_len - 1);
    } else {
        editor.move_position(start);
    }
}

fn yank_selection(editor: &mut HexEditor) {
    if let Some((start, end)) = editor.selection_range() {
        let bytes = editor
            .tab()
            .document
            .get_slice(start..end + 1)
            .unwrap_or_default();
        let len = bytes.len();
        editor.tab_mut().vim_state.yank_buffer = bytes;
        editor.log(
            crate::log_panel::LogLevel::Info,
            &format!("Yanked {} bytes", len),
        );
    }
}

fn delete_selection(editor: &mut HexEditor) {
    if let Some((start, end)) = editor.selection_range() {
        let count = end - start + 1;
        editor.clear_selection();
        delete_range(editor, start, count);
    }
}

fn paste_bytes(editor: &mut HexEditor, after: bool) {
    let buf = editor.tab().vim_state.yank_buffer.clone();
    if buf.is_empty() {
        return;
    }

    let pos = if after {
        // p: paste after cursor
        (editor.tab().cursor_position + 1).min(editor.tab().document.len())
    } else {
        // P: paste before cursor
        editor.tab().cursor_position
    };

    if let Err(e) = editor.tab_mut().document.insert_bytes(pos, &buf) {
        eprintln!("Failed to insert bytes: {}", e);
        return;
    }
    editor.invalidate_render_cache();
    editor.move_position(pos + buf.len() - 1);
    editor.log(
        crate::log_panel::LogLevel::Info,
        &format!("Pasted {} bytes", buf.len()),
    );
}

// ─── Tests ─────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn vim_state_new_defaults_to_normal() {
        let state = VimState::new();
        assert_eq!(state.mode, VimMode::Normal);
        assert!(state.count.is_none());
        assert!(state.pending_operator.is_none());
        assert!(state.pending_key.is_none());
        assert!(state.command_buffer.is_empty());
        assert!(state.yank_buffer.is_empty());
    }

    #[test]
    fn enter_mode_resets_pending() {
        let mut state = VimState::new();
        state.count = Some(5);
        state.pending_operator = Some(VimOperator::Delete);
        state.pending_key = Some('g');

        state.enter_mode(VimMode::Insert);
        assert_eq!(state.mode, VimMode::Insert);
        assert!(state.count.is_none());
        assert!(state.pending_operator.is_none());
        assert!(state.pending_key.is_none());
    }

    #[test]
    fn accumulate_count_builds_number() {
        let mut state = VimState::new();
        state.accumulate_count(3);
        assert_eq!(state.count, Some(3));
        state.accumulate_count(2);
        assert_eq!(state.count, Some(32));
        state.accumulate_count(1);
        assert_eq!(state.count, Some(321));
    }

    #[test]
    fn take_count_returns_default_1() {
        let mut state = VimState::new();
        assert_eq!(state.take_count(), 1);
        assert!(state.count.is_none());
    }

    #[test]
    fn take_count_returns_accumulated() {
        let mut state = VimState::new();
        state.accumulate_count(5);
        assert_eq!(state.take_count(), 5);
        assert!(state.count.is_none());
    }

    #[test]
    fn reset_pending_clears_all() {
        let mut state = VimState::new();
        state.count = Some(10);
        state.pending_operator = Some(VimOperator::Yank);
        state.pending_key = Some('r');

        state.reset_pending();
        assert!(state.count.is_none());
        assert!(state.pending_operator.is_none());
        assert!(state.pending_key.is_none());
    }

    #[test]
    fn enter_command_mode_clears_buffer() {
        let mut state = VimState::new();
        state.command_buffer = "test".to_string();
        // Entering command mode keeps buffer (cleared by `:` handler before enter_mode)
        // But enter_mode for non-Command clears it
        state.enter_mode(VimMode::Normal);
        assert!(state.command_buffer.is_empty());

        // Re-entering Command mode: command_buffer is only cleared by non-Command modes
        state.mode = VimMode::Command;
        state.command_buffer = "wq".to_string();
        state.enter_mode(VimMode::Command);
        // Command mode doesn't clear command_buffer itself
        assert_eq!(state.command_buffer, "wq");
    }

    #[test]
    fn enter_normal_from_command_clears_buffer() {
        let mut state = VimState::new();
        state.mode = VimMode::Command;
        state.command_buffer = "wq".to_string();
        state.enter_mode(VimMode::Normal);
        assert!(state.command_buffer.is_empty());
    }

    #[test]
    fn parse_hex_address() {
        assert_eq!(parse_address("0xFF"), Some(0xFF));
        assert_eq!(parse_address("0X1A2B"), Some(0x1A2B));
        assert_eq!(parse_address("0x0"), Some(0));
    }

    #[test]
    fn parse_decimal_address() {
        assert_eq!(parse_address("123"), Some(123));
        assert_eq!(parse_address("0"), Some(0));
    }

    #[test]
    fn parse_invalid_address() {
        assert_eq!(parse_address("abc"), None);
        assert_eq!(parse_address(""), None);
        assert_eq!(parse_address("0xZZ"), None);
    }
}
