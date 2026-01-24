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

/// Byte order for multi-byte value interpretation
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum Endian {
    /// Little-endian (least significant byte first)
    #[default]
    Little,
    /// Big-endian (most significant byte first)
    Big,
}

/// Text encoding for ASCII column display
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum TextEncoding {
    /// ASCII (0x20-0x7E only)
    #[default]
    Ascii,
    /// Latin-1 (ISO-8859-1, 0x00-0xFF as single bytes)
    Latin1,
    /// UTF-8 (1-4 bytes per character)
    Utf8,
    /// Shift-JIS (Japanese, 1-2 bytes per character)
    ShiftJis,
    /// EUC-JP (Japanese, 1-3 bytes per character)
    EucJp,
}

impl TextEncoding {
    /// Get display label for this encoding
    pub fn label(&self) -> &'static str {
        match self {
            TextEncoding::Ascii => "ASCII",
            TextEncoding::Latin1 => "Latin1",
            TextEncoding::Utf8 => "UTF-8",
            TextEncoding::ShiftJis => "SJIS",
            TextEncoding::EucJp => "EUC",
        }
    }

    /// Get all available encodings
    pub fn all() -> &'static [TextEncoding] {
        &[
            TextEncoding::Ascii,
            TextEncoding::Latin1,
            TextEncoding::Utf8,
            TextEncoding::ShiftJis,
            TextEncoding::EucJp,
        ]
    }

    /// Cycle to next encoding
    pub fn next(&self) -> TextEncoding {
        match self {
            TextEncoding::Ascii => TextEncoding::Latin1,
            TextEncoding::Latin1 => TextEncoding::Utf8,
            TextEncoding::Utf8 => TextEncoding::ShiftJis,
            TextEncoding::ShiftJis => TextEncoding::EucJp,
            TextEncoding::EucJp => TextEncoding::Ascii,
        }
    }
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

/// Data inspector interpretation results
pub struct DataInspectorValues {
    pub int8: i8,
    pub uint8: u8,
    pub int16: Option<i16>,
    pub uint16: Option<u16>,
    pub int32: Option<i32>,
    pub uint32: Option<u32>,
    pub int64: Option<i64>,
    pub uint64: Option<u64>,
    pub float32: Option<f32>,
    pub float64: Option<f64>,
}

impl DataInspectorValues {
    /// Create data inspector values from bytes at cursor position
    ///
    /// # Arguments
    /// * `get_byte` - Function to get byte at position
    /// * `cursor_pos` - Current cursor position
    /// * `doc_len` - Total document length
    /// * `endian` - Byte order for multi-byte values
    pub fn from_bytes<F>(get_byte: F, cursor_pos: usize, doc_len: usize, endian: Endian) -> Option<Self>
    where
        F: Fn(usize) -> Option<u8>,
    {
        let b0 = get_byte(cursor_pos)?;

        // Get bytes for multi-byte values
        let bytes_available = doc_len.saturating_sub(cursor_pos);

        let b1 = if bytes_available >= 2 { get_byte(cursor_pos + 1) } else { None };
        let b2 = if bytes_available >= 3 { get_byte(cursor_pos + 2) } else { None };
        let b3 = if bytes_available >= 4 { get_byte(cursor_pos + 3) } else { None };
        let b4 = if bytes_available >= 5 { get_byte(cursor_pos + 4) } else { None };
        let b5 = if bytes_available >= 6 { get_byte(cursor_pos + 5) } else { None };
        let b6 = if bytes_available >= 7 { get_byte(cursor_pos + 6) } else { None };
        let b7 = if bytes_available >= 8 { get_byte(cursor_pos + 7) } else { None };

        // 16-bit values
        let (int16, uint16) = if let Some(b1) = b1 {
            let bytes = match endian {
                Endian::Little => [b0, b1],
                Endian::Big => [b1, b0],
            };
            let val = match endian {
                Endian::Little => i16::from_le_bytes(bytes),
                Endian::Big => i16::from_be_bytes([b0, b1]),
            };
            let uval = match endian {
                Endian::Little => u16::from_le_bytes(bytes),
                Endian::Big => u16::from_be_bytes([b0, b1]),
            };
            (Some(val), Some(uval))
        } else {
            (None, None)
        };

        // 32-bit values
        let (int32, uint32, float32) = if let (Some(b1), Some(b2), Some(b3)) = (b1, b2, b3) {
            let bytes = match endian {
                Endian::Little => [b0, b1, b2, b3],
                Endian::Big => [b3, b2, b1, b0],
            };
            let val = match endian {
                Endian::Little => i32::from_le_bytes(bytes),
                Endian::Big => i32::from_be_bytes([b0, b1, b2, b3]),
            };
            let uval = match endian {
                Endian::Little => u32::from_le_bytes(bytes),
                Endian::Big => u32::from_be_bytes([b0, b1, b2, b3]),
            };
            let fval = match endian {
                Endian::Little => f32::from_le_bytes(bytes),
                Endian::Big => f32::from_be_bytes([b0, b1, b2, b3]),
            };
            (Some(val), Some(uval), Some(fval))
        } else {
            (None, None, None)
        };

        // 64-bit values
        let (int64, uint64, float64) = if let (Some(b1), Some(b2), Some(b3), Some(b4), Some(b5), Some(b6), Some(b7)) = (b1, b2, b3, b4, b5, b6, b7) {
            let bytes = match endian {
                Endian::Little => [b0, b1, b2, b3, b4, b5, b6, b7],
                Endian::Big => [b7, b6, b5, b4, b3, b2, b1, b0],
            };
            let val = match endian {
                Endian::Little => i64::from_le_bytes(bytes),
                Endian::Big => i64::from_be_bytes([b0, b1, b2, b3, b4, b5, b6, b7]),
            };
            let uval = match endian {
                Endian::Little => u64::from_le_bytes(bytes),
                Endian::Big => u64::from_be_bytes([b0, b1, b2, b3, b4, b5, b6, b7]),
            };
            let fval = match endian {
                Endian::Little => f64::from_le_bytes(bytes),
                Endian::Big => f64::from_be_bytes([b0, b1, b2, b3, b4, b5, b6, b7]),
            };
            (Some(val), Some(uval), Some(fval))
        } else {
            (None, None, None)
        };

        Some(Self {
            int8: b0 as i8,
            uint8: b0,
            int16,
            uint16,
            int32,
            uint32,
            int64,
            uint64,
            float32,
            float64,
        })
    }
}

/// Default row height constant in pixels (content: 20px + margin-bottom mb_1: 4px)
/// This is kept for documentation and potential fallback use.
/// Actual row height is calculated from font metrics (ascent + descent + margin).
#[allow(dead_code)]
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
/// * `row_height` - Height of each row in pixels (font_size + margin)
pub fn calculate_visible_range(
    scroll_offset: Pixels,
    content_height: Pixels,
    total_rows: usize,
    row_height: f64,
) -> VisibleRange {
    if total_rows == 0 {
        return VisibleRange { render_start: 0, render_end: 0, visible_rows: 20 };
    }

    // Use f64 for calculations to avoid precision loss
    let scroll_offset_f64: f64 = scroll_offset.into();
    let content_height_f64: f64 = f64::from(content_height).max(row_height);

    // Calculate the actual total content height
    let actual_total_height = total_rows as f64 * row_height;

    // Calculate first visible row using ratio if content is very large
    let first_visible_row = if actual_total_height > MAX_VIRTUAL_HEIGHT {
        let scroll_ratio = (-scroll_offset_f64) / MAX_VIRTUAL_HEIGHT;
        let scroll_ratio = scroll_ratio.clamp(0.0, 1.0);
        (scroll_ratio * total_rows as f64).floor() as usize
    } else {
        let scroll_offset_abs = -scroll_offset_f64;
        if scroll_offset_abs > 0.0 {
            (scroll_offset_abs / row_height).floor() as usize
        } else {
            0
        }
    };

    // Calculate number of visible rows (actual rows that fit in viewport)
    let visible_row_count = (content_height_f64 / row_height).floor() as usize;
    let visible_row_count = visible_row_count.max(5); // Minimum rows for rendering

    // Add buffer rows to prevent flickering during scroll
    let buffer_rows = 10;
    // Additional margin for partial rows and layout differences
    let render_margin = 4;

    // Clamp first_visible_row to ensure we can render full viewport at the end
    let max_first_row = total_rows.saturating_sub(visible_row_count);
    let first_visible_row = first_visible_row.min(max_first_row);

    let render_start = first_visible_row.saturating_sub(buffer_rows);
    let render_end = (first_visible_row + visible_row_count + buffer_rows + render_margin).min(total_rows);

    VisibleRange {
        render_start,
        render_end,
        visible_rows: visible_row_count,
    }
}

/// Calculate the first and last visible row from scroll offset.
///
/// Arguments:
/// - current_offset: Current Y scroll offset (negative when scrolled down)
/// - visible_rows: Number of rows visible in the content area
/// - total_rows: Total number of rows in the document
/// - row_height: Height of each row in pixels (font_size + margin)
///
/// Returns: (first_visible_row, last_visible_row)
pub fn calculate_visible_row_range(
    current_offset: Pixels,
    visible_rows: usize,
    total_rows: usize,
    row_height: f64,
) -> (usize, usize) {
    if total_rows == 0 || visible_rows == 0 {
        return (0, 0);
    }

    let current_offset_f64: f64 = current_offset.into();
    let actual_total_height = total_rows as f64 * row_height;

    // Calculate first visible row from scroll offset
    // Use floor() to match calculate_visible_range
    let first_visible_row = if actual_total_height > MAX_VIRTUAL_HEIGHT {
        let scroll_ratio = (-current_offset_f64) / MAX_VIRTUAL_HEIGHT;
        let scroll_ratio = scroll_ratio.clamp(0.0, 1.0);
        (scroll_ratio * total_rows as f64).floor() as usize
    } else {
        let scroll_offset_abs = -current_offset_f64;
        if scroll_offset_abs > 0.0 {
            (scroll_offset_abs / row_height).floor() as usize
        } else {
            0
        }
    };

    // Clamp first_visible_row to match calculate_visible_range behavior
    let max_first_row = total_rows.saturating_sub(visible_rows);
    let first_visible_row = first_visible_row.min(max_first_row);

    // Last visible row calculation
    let last_visible_row = first_visible_row + visible_rows.saturating_sub(1);

    (first_visible_row, last_visible_row)
}

/// Calculate scroll offset from target row.
///
/// Arguments:
/// - target_row: The row to position at the top of the visible area
/// - visible_rows: Number of rows visible in the content area
/// - total_rows: Total number of rows in the document
/// - row_height: Height of each row in pixels (font_size + margin)
///
/// Returns: Scroll offset in pixels (negative value)
pub fn calculate_scroll_offset(
    target_row: usize,
    visible_rows: usize,
    total_rows: usize,
    row_height: f64,
) -> Pixels {
    let actual_total_height = total_rows as f64 * row_height;

    // Clamp target_row so that the last row appears at the bottom of visible area
    let max_target_row = total_rows.saturating_sub(visible_rows);
    let target_row = target_row.min(max_target_row);

    // Calculate offset using integer row position to avoid float drift
    // Use floor() to match calculate_visible_row_range calculation
    if actual_total_height > MAX_VIRTUAL_HEIGHT {
        let ratio = target_row as f64 / total_rows as f64;
        let offset = (ratio * MAX_VIRTUAL_HEIGHT).floor();
        px(-offset as f32)
    } else {
        // Use floor() to ensure consistency with visible row calculation
        let offset = (target_row as f64 * row_height).floor();
        px(-offset as f32)
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
/// - row_height: Height of each row in pixels (font_size + margin)
pub fn calculate_scroll_to_row(
    cursor_row: usize,
    current_offset: Pixels,
    visible_rows: usize,
    total_rows: usize,
    row_height: f64,
) -> Option<Pixels> {
    if total_rows == 0 || visible_rows == 0 {
        return None;
    }

    let (first_visible_row, last_visible_row) =
        calculate_visible_row_range(current_offset, visible_rows, total_rows, row_height);

    // Check if cursor is already visible
    if cursor_row >= first_visible_row && cursor_row <= last_visible_row {
        return None;
    }

    // Calculate target row (first visible row after scrolling)
    let target_row = if cursor_row < first_visible_row {
        // Scrolling up
        let distance = first_visible_row.saturating_sub(cursor_row);
        if distance <= 1 {
            // Small movement (e.g., Up key): scroll by 1 row for smooth scrolling
            first_visible_row.saturating_sub(1)
        } else {
            // Large jump (beyond screen): put cursor at top
            cursor_row
        }
    } else {
        // Scrolling down
        let distance = cursor_row.saturating_sub(last_visible_row);
        if distance <= 1 {
            // Small movement (e.g., Down key): scroll by 1 row for smooth scrolling
            first_visible_row + 1
        } else {
            // Large jump (e.g., Ctrl+End): position cursor at bottom of visible area
            cursor_row.saturating_sub(visible_rows.saturating_sub(1))
        }
    };

    Some(calculate_scroll_offset(target_row, visible_rows, total_rows, row_height))
}

/// Calculate spacer heights for virtual scrolling
/// Uses capped virtual height to avoid f32 precision issues
///
/// Arguments:
/// - render_start: First row index to render
/// - render_end: Last row index to render (exclusive)
/// - total_rows: Total number of rows in the document
/// - row_height: Height of each row in pixels (font_size + margin)
pub fn calculate_spacer_heights(
    render_start: usize,
    render_end: usize,
    total_rows: usize,
    row_height: f64,
) -> (Pixels, Pixels) {
    if total_rows == 0 {
        return (px(0.0), px(0.0));
    }

    let actual_total_height = total_rows as f64 * row_height;

    if actual_total_height > MAX_VIRTUAL_HEIGHT {
        // Use ratio-based spacer heights for large files
        let top_ratio = render_start as f64 / total_rows as f64;
        let bottom_ratio = (total_rows - render_end) as f64 / total_rows as f64;

        let top_height = (top_ratio * MAX_VIRTUAL_HEIGHT) as f32;
        let bottom_height = (bottom_ratio * MAX_VIRTUAL_HEIGHT) as f32;

        (px(top_height), px(bottom_height))
    } else {
        // Direct calculation for smaller files
        let top_height = (render_start as f64 * row_height) as f32;
        let bottom_height = ((total_rows - render_end) as f64 * row_height) as f32;

        (px(top_height), px(bottom_height))
    }
}
