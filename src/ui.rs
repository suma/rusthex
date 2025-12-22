//! UI types and utilities for the hex editor
//!
//! This module contains UI-related types, enums, and helper functions
//! for rendering and display calculations.

use gpui::Pixels;

/// Edit pane selection (Hex or ASCII)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EditPane {
    /// Hex editing mode
    Hex,
    /// ASCII editing mode
    Ascii,
}

/// Hex nibble position tracker
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HexNibble {
    /// Upper nibble (first hex digit)
    High,
    /// Lower nibble (second hex digit)
    Low,
}

/// Format byte offset as 8-digit hexadecimal address
pub fn format_address(offset: usize) -> String {
    format!("{:08X}", offset)
}

/// Calculate row count from document length
pub fn row_count(doc_len: usize, bytes_per_row: usize) -> usize {
    (doc_len + bytes_per_row - 1) / bytes_per_row
}

/// Format byte value as hexadecimal
pub fn format_byte_hex(byte: u8) -> String {
    format!("0x{:02X}", byte)
}

/// Format byte value as decimal
pub fn format_byte_dec(byte: u8) -> String {
    format!("{}", byte)
}

/// Format byte value as binary
pub fn format_byte_bin(byte: u8) -> String {
    format!("{:08b}", byte)
}

/// Format file size with appropriate unit
pub fn format_file_size(size: usize) -> String {
    if size < 1024 {
        format!("{} B", size)
    } else if size < 1024 * 1024 {
        format!("{:.1} KB", size as f64 / 1024.0)
    } else if size < 1024 * 1024 * 1024 {
        format!("{:.1} MB", size as f64 / (1024.0 * 1024.0))
    } else {
        format!("{:.1} GB", size as f64 / (1024.0 * 1024.0 * 1024.0))
    }
}

/// Calculate visible row range for virtual scrolling
///
/// Returns (render_start, render_end) tuple indicating which rows should be rendered
pub fn calculate_visible_range(
    scroll_offset: Pixels,
    viewport_height: Pixels,
    total_rows: usize,
    _bytes_per_row: usize,
) -> (usize, usize) {
    let row_height = Pixels::from(20.0);

    // Calculate first visible row
    // Note: scroll_offset is negative when scrolling down in gpui
    let scroll_offset_abs = scroll_offset * -1.0;
    let first_visible_row = if scroll_offset_abs > Pixels::from(0.0) {
        (scroll_offset_abs / row_height).floor() as usize
    } else {
        0
    };

    // Calculate number of visible rows (minimum 50 to handle edge cases)
    let visible_row_count = (viewport_height / row_height).ceil() as usize;
    let visible_row_count = visible_row_count.max(50); // Ensure minimum rows

    // Add buffer rows to prevent flickering during scroll
    let buffer_rows = 10;
    let render_start = first_visible_row.saturating_sub(buffer_rows);
    let mut render_end = (first_visible_row + visible_row_count + buffer_rows).min(total_rows);

    // Ensure we render at least visible_row_count rows
    if render_end < render_start + visible_row_count {
        render_end = (render_start + visible_row_count).min(total_rows);
    }

    (render_start, render_end)
}
