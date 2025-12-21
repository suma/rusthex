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

    // Calculate number of visible rows
    let visible_row_count = (viewport_height / row_height).ceil() as usize;

    // Add buffer rows to prevent flickering during scroll
    let buffer_rows = 5;
    let mut render_start = first_visible_row.saturating_sub(buffer_rows);
    let render_end = (first_visible_row + visible_row_count + buffer_rows).min(total_rows);

    // Ensure we render at least visible_row_count rows when near the end
    if render_end == total_rows {
        // At the end of the file, adjust render_start to show full viewport
        let desired_render_count = visible_row_count + buffer_rows * 2;
        render_start = total_rows.saturating_sub(desired_render_count);
    }

    (render_start, render_end)
}
