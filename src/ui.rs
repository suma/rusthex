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

/// Row height constant in pixels (content: 20px + margin-bottom mb_1: 4px)
pub const ROW_HEIGHT: f64 = 24.0;

/// Maximum virtual height to avoid f32 precision issues
/// f32 loses precision above ~16 million, so we cap at 10 million pixels
const MAX_VIRTUAL_HEIGHT: f64 = 10_000_000.0;

/// Result of visible range calculation
pub struct VisibleRange {
    pub render_start: usize,
    pub render_end: usize,
    pub visible_rows: usize, // Actual number of rows visible in viewport (without buffer)
}

/// Calculate visible row range for virtual scrolling
///
/// Returns VisibleRange with render bounds and actual visible row count
/// Uses f64 internally to avoid precision loss with large files
///
/// # Arguments
/// * `scroll_offset` - Current scroll offset (negative when scrolled down)
/// * `content_height` - Height of content area in pixels (excluding header/status bar)
/// * `total_rows` - Total number of rows in the document
/// * `_bytes_per_row` - Bytes per row (unused, kept for API compatibility)
pub fn calculate_visible_range(
    scroll_offset: Pixels,
    content_height: Pixels,
    total_rows: usize,
    _bytes_per_row: usize,
) -> VisibleRange {
    if total_rows == 0 {
        return VisibleRange { render_start: 0, render_end: 0, visible_rows: 20 };
    }

    // Use f64 for calculations to avoid precision loss
    let scroll_offset_f64: f64 = scroll_offset.into();
    let content_height_f64: f64 = f64::from(content_height).max(ROW_HEIGHT);

    // Calculate the actual total content height
    let actual_total_height = total_rows as f64 * ROW_HEIGHT;

    // Calculate first visible row using ratio if content is very large
    let first_visible_row = if actual_total_height > MAX_VIRTUAL_HEIGHT {
        let scroll_ratio = (-scroll_offset_f64) / MAX_VIRTUAL_HEIGHT;
        let scroll_ratio = scroll_ratio.clamp(0.0, 1.0);
        (scroll_ratio * total_rows as f64).floor() as usize
    } else {
        let scroll_offset_abs = -scroll_offset_f64;
        if scroll_offset_abs > 0.0 {
            (scroll_offset_abs / ROW_HEIGHT).floor() as usize
        } else {
            0
        }
    };

    // Calculate number of fully visible rows (use floor to exclude partial rows)
    // This is used for cursor visibility - we only count rows that are completely visible
    let visible_row_count = (content_height_f64 / ROW_HEIGHT).floor() as usize;
    let visible_row_count = visible_row_count.max(5); // Minimum rows for rendering

    // Add buffer rows to prevent flickering during scroll
    let buffer_rows = 10;

    // Clamp first_visible_row to ensure we can render full viewport at the end
    let max_first_row = total_rows.saturating_sub(visible_row_count);
    let first_visible_row = first_visible_row.min(max_first_row);

    let render_start = first_visible_row.saturating_sub(buffer_rows);
    let render_end = (first_visible_row + visible_row_count + buffer_rows).min(total_rows);

    VisibleRange {
        render_start,
        render_end,
        visible_rows: visible_row_count,
    }
}

/// Calculate the scroll offset needed to make a row visible
/// Returns Some(new_offset) if scrolling is needed, None if the row is already visible
///
/// Arguments:
/// - cursor_row: The row to make visible
/// - current_offset: Current Y scroll offset (negative when scrolled down)
/// - visible_rows: Number of rows visible in the content area
/// - total_rows: Total number of rows in the document
pub fn calculate_scroll_to_row(
    cursor_row: usize,
    current_offset: Pixels,
    visible_rows: usize,
    total_rows: usize,
) -> Option<Pixels> {
    if total_rows == 0 || visible_rows == 0 {
        return None;
    }

    let current_offset_f64: f64 = current_offset.into();
    let actual_total_height = total_rows as f64 * ROW_HEIGHT;

    // Calculate first visible row from scroll offset
    // Use floor() to match calculate_visible_range
    let first_visible_row = if actual_total_height > MAX_VIRTUAL_HEIGHT {
        let scroll_ratio = (-current_offset_f64) / MAX_VIRTUAL_HEIGHT;
        let scroll_ratio = scroll_ratio.clamp(0.0, 1.0);
        (scroll_ratio * total_rows as f64).floor() as usize
    } else {
        let scroll_offset_abs = -current_offset_f64;
        if scroll_offset_abs > 0.0 {
            (scroll_offset_abs / ROW_HEIGHT).floor() as usize
        } else {
            0
        }
    };

    // Clamp first_visible_row to match calculate_visible_range behavior
    let max_first_row = total_rows.saturating_sub(visible_rows);
    let first_visible_row = first_visible_row.min(max_first_row);

    // Last visible row
    let last_visible_row = first_visible_row + visible_rows.saturating_sub(1);

    // Check if cursor is already visible
    if cursor_row >= first_visible_row && cursor_row <= last_visible_row {
        return None;
    }

    // Calculate new scroll offset
    let target_row = if cursor_row < first_visible_row {
        // Scrolling up: put cursor at top
        cursor_row
    } else {
        // Scrolling down: scroll just enough to show cursor at bottom
        cursor_row.saturating_sub(visible_rows.saturating_sub(1))
    };

    // Don't clamp target_row too aggressively - allow scrolling to show last row
    let max_target_row = total_rows.saturating_sub(1);
    let target_row = target_row.min(max_target_row);

    // Calculate new offset using integer row position to avoid float drift
    // Round to exact row boundary to prevent accumulating errors
    let new_offset = if actual_total_height > MAX_VIRTUAL_HEIGHT {
        let ratio = target_row as f64 / total_rows as f64;
        let offset = (ratio * MAX_VIRTUAL_HEIGHT).round();
        px(-offset as f32)
    } else {
        // Use exact integer multiplication to avoid float errors
        let offset = (target_row as f64 * ROW_HEIGHT).round();
        px(-offset as f32)
    };

    Some(new_offset)
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
