//! UI types and utilities for the hex editor
//!
//! This module contains UI-related types, enums, and helper functions
//! for rendering and display calculations.

use gpui::{Pixels, px};

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

/// Row height constant in pixels
pub const ROW_HEIGHT: f64 = 20.0;

/// Maximum virtual height to avoid f32 precision issues
/// f32 loses precision above ~16 million, so we cap at 10 million pixels
const MAX_VIRTUAL_HEIGHT: f64 = 10_000_000.0;

/// Calculate visible row range for virtual scrolling
///
/// Returns (render_start, render_end) tuple indicating which rows should be rendered
/// Uses f64 internally to avoid precision loss with large files
pub fn calculate_visible_range(
    scroll_offset: Pixels,
    viewport_height: Pixels,
    total_rows: usize,
    _bytes_per_row: usize,
) -> (usize, usize) {
    if total_rows == 0 {
        return (0, 0);
    }

    // Use f64 for calculations to avoid precision loss
    // Convert Pixels to f64 via f32
    let scroll_offset_f64: f64 = scroll_offset.into();
    let viewport_height_f64: f64 = f64::from(viewport_height).max(400.0); // Minimum viewport

    // Calculate the actual total content height
    let actual_total_height = total_rows as f64 * ROW_HEIGHT;

    // Calculate first visible row using ratio if content is very large
    let first_visible_row = if actual_total_height > MAX_VIRTUAL_HEIGHT {
        // Use ratio-based calculation for large files
        // scroll_offset is negative when scrolling down
        let scroll_ratio = (-scroll_offset_f64) / MAX_VIRTUAL_HEIGHT;
        let scroll_ratio = scroll_ratio.clamp(0.0, 1.0);
        (scroll_ratio * total_rows as f64).floor() as usize
    } else {
        // Direct calculation for smaller files
        let scroll_offset_abs = -scroll_offset_f64;
        if scroll_offset_abs > 0.0 {
            (scroll_offset_abs / ROW_HEIGHT).floor() as usize
        } else {
            0
        }
    };

    // Calculate number of visible rows
    let visible_row_count = (viewport_height_f64 / ROW_HEIGHT).ceil() as usize;
    let visible_row_count = visible_row_count.max(50); // Ensure minimum rows

    // Add buffer rows to prevent flickering during scroll
    let buffer_rows = 10;

    // Clamp first_visible_row to ensure we can render full viewport at the end
    let max_first_row = total_rows.saturating_sub(visible_row_count);
    let first_visible_row = first_visible_row.min(max_first_row);

    let render_start = first_visible_row.saturating_sub(buffer_rows);
    let render_end = (first_visible_row + visible_row_count + buffer_rows).min(total_rows);

    (render_start, render_end)
}

/// Calculate spacer heights for virtual scrolling
/// Uses capped virtual height to avoid f32 precision issues
pub fn calculate_spacer_heights(
    render_start: usize,
    render_end: usize,
    total_rows: usize,
) -> (Pixels, Pixels) {
    if total_rows == 0 {
        return (px(0.0), px(0.0));
    }

    let actual_total_height = total_rows as f64 * ROW_HEIGHT;

    if actual_total_height > MAX_VIRTUAL_HEIGHT {
        // Use ratio-based spacer heights for large files
        let top_ratio = render_start as f64 / total_rows as f64;
        let bottom_ratio = (total_rows - render_end) as f64 / total_rows as f64;

        let top_height = (top_ratio * MAX_VIRTUAL_HEIGHT) as f32;
        let bottom_height = (bottom_ratio * MAX_VIRTUAL_HEIGHT) as f32;

        (px(top_height), px(bottom_height))
    } else {
        // Direct calculation for smaller files
        let top_height = (render_start as f64 * ROW_HEIGHT) as f32;
        let bottom_height = ((total_rows - render_end) as f64 * ROW_HEIGHT) as f32;

        (px(top_height), px(bottom_height))
    }
}
