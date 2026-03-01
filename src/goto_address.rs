//! Go to Address functionality for the hex editor
//!
//! Provides address parsing and navigation to arbitrary byte offsets.
//! Supports decimal (default) and hexadecimal (0x prefix) input.

use crate::HexEditor;

/// Parse an address string into a byte offset.
///
/// Supports:
/// - Decimal (default): "100" → 100
/// - Hexadecimal (0x prefix): "0xFF" → 255
///
/// Returns `None` for empty or invalid input.
pub fn parse_address(input: &str) -> Option<usize> {
    let trimmed = input.trim();
    if trimmed.is_empty() {
        return None;
    }

    if let Some(hex_str) = trimmed.strip_prefix("0x").or_else(|| trimmed.strip_prefix("0X")) {
        if hex_str.is_empty() {
            return None;
        }
        usize::from_str_radix(hex_str, 16).ok()
    } else {
        trimmed.parse::<usize>().ok()
    }
}

impl HexEditor {
    /// Show the Go to Address bar, closing other input bars (search, bookmark comment).
    pub fn show_goto_address(&mut self) {
        // Close other input bars (mutual exclusion)
        self.tab_mut().search_visible = false;
        self.tab_mut().search_results.clear();
        self.tab_mut().current_search_index = None;
        self.tab_mut().bookmark_comment_editing = false;

        self.tab_mut().goto_address_visible = true;
        self.tab_mut().goto_address_text.clear();
    }

    /// Hide the Go to Address bar.
    pub fn hide_goto_address(&mut self) {
        self.tab_mut().goto_address_visible = false;
        self.tab_mut().goto_address_text.clear();
    }

    /// Parse the entered address, move the cursor there, and close the bar.
    /// If the address exceeds file length, clamps to the last byte.
    pub fn confirm_goto_address(&mut self) {
        let input = self.tab().goto_address_text.clone();
        if let Some(addr) = parse_address(&input) {
            let doc_len = self.tab().document.len();
            let target = if doc_len == 0 { 0 } else { addr.min(doc_len - 1) };
            self.push_cursor_history();
            self.move_position(target);
            self.log(
                crate::log_panel::LogLevel::Info,
                format!("Go to address 0x{:X} ({})", target, target),
            );
        } else {
            self.log(
                crate::log_panel::LogLevel::Error,
                format!("Invalid address: {}", input),
            );
        }
        self.hide_goto_address();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_decimal() {
        assert_eq!(parse_address("100"), Some(100));
        assert_eq!(parse_address("0"), Some(0));
        assert_eq!(parse_address("1000"), Some(1000));
        assert_eq!(parse_address("255"), Some(255));
    }

    #[test]
    fn test_parse_hex_prefix() {
        assert_eq!(parse_address("0xFF"), Some(255));
        assert_eq!(parse_address("0x100"), Some(256));
        assert_eq!(parse_address("0x0"), Some(0));
        assert_eq!(parse_address("0xDEAD"), Some(0xDEAD));
    }

    #[test]
    fn test_parse_hex_uppercase_prefix() {
        assert_eq!(parse_address("0XFF"), Some(255));
        assert_eq!(parse_address("0X100"), Some(256));
    }

    #[test]
    fn test_parse_with_whitespace() {
        assert_eq!(parse_address("  100  "), Some(100));
        assert_eq!(parse_address("  0xFF  "), Some(255));
    }

    #[test]
    fn test_parse_empty() {
        assert_eq!(parse_address(""), None);
        assert_eq!(parse_address("  "), None);
    }

    #[test]
    fn test_parse_invalid() {
        assert_eq!(parse_address("abc"), None);
        assert_eq!(parse_address("0x"), None);
        assert_eq!(parse_address("0xGG"), None);
        assert_eq!(parse_address("-1"), None);
    }
}
