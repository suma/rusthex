//! UI types and utilities for the hex editor
//!
//! This module contains UI-related types, enums, and helper functions
//! for rendering and display calculations.

use gpui::{Pixels, px};

/// Edit pane selection (Hex, ASCII, or Bitmap)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum EditPane {
    /// Hex editing mode
    Hex,
    /// ASCII editing mode
    Ascii,
    /// Bitmap viewport indicator drag mode
    Bitmap,
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
    /// UTF-16 Big-Endian (2 or 4 bytes per character)
    Utf16Be,
    /// UTF-16 Little-Endian (2 or 4 bytes per character)
    Utf16Le,
    /// UTF-32 Big-Endian (4 bytes per character)
    Utf32Be,
    /// UTF-32 Little-Endian (4 bytes per character)
    Utf32Le,
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
            TextEncoding::Utf16Be => "UTF16BE",
            TextEncoding::Utf16Le => "UTF16LE",
            TextEncoding::Utf32Be => "UTF32BE",
            TextEncoding::Utf32Le => "UTF32LE",
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
            TextEncoding::Utf16Be,
            TextEncoding::Utf16Le,
            TextEncoding::Utf32Be,
            TextEncoding::Utf32Le,
        ]
    }

    /// Cycle to next encoding
    pub fn next(&self) -> TextEncoding {
        match self {
            TextEncoding::Ascii => TextEncoding::Latin1,
            TextEncoding::Latin1 => TextEncoding::Utf8,
            TextEncoding::Utf8 => TextEncoding::ShiftJis,
            TextEncoding::ShiftJis => TextEncoding::EucJp,
            TextEncoding::EucJp => TextEncoding::Utf16Be,
            TextEncoding::Utf16Be => TextEncoding::Utf16Le,
            TextEncoding::Utf16Le => TextEncoding::Utf32Be,
            TextEncoding::Utf32Be => TextEncoding::Utf32Le,
            TextEncoding::Utf32Le => TextEncoding::Ascii,
        }
    }
}

/// Edit mode (Overwrite or Insert)
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum EditMode {
    /// Overwrite mode: replace existing bytes
    #[default]
    Overwrite,
    /// Insert mode: insert new bytes at cursor position
    Insert,
}

/// Hex nibble position tracker
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum HexNibble {
    /// Upper nibble (first hex digit)
    High,
    /// Lower nibble (second hex digit)
    Low,
}

/// Calculate the number of hex characters needed to display addresses for a given document size.
/// Returns 8 for files <= 4GB, 12 for files <= 16TB, 16 otherwise.
pub fn address_chars(doc_len: usize) -> usize {
    if doc_len <= 0xFFFF_FFFF {
        8
    } else if doc_len <= 0xFFFF_FFFF_FFFF {
        12
    } else {
        16
    }
}

/// Format byte offset as a hexadecimal address with the given number of digits.
pub fn format_address(offset: usize, chars: usize) -> String {
    match chars {
        12 => format!("{:012X}", offset),
        16 => format!("{:016X}", offset),
        _ => format!("{:08X}", offset),
    }
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
    pub fn from_bytes<F>(
        get_byte: F,
        cursor_pos: usize,
        doc_len: usize,
        endian: Endian,
    ) -> Option<Self>
    where
        F: Fn(usize) -> Option<u8>,
    {
        let b0 = get_byte(cursor_pos)?;

        // Get bytes for multi-byte values
        let bytes_available = doc_len.saturating_sub(cursor_pos);

        let b1 = if bytes_available >= 2 {
            get_byte(cursor_pos + 1)
        } else {
            None
        };
        let b2 = if bytes_available >= 3 {
            get_byte(cursor_pos + 2)
        } else {
            None
        };
        let b3 = if bytes_available >= 4 {
            get_byte(cursor_pos + 3)
        } else {
            None
        };
        let b4 = if bytes_available >= 5 {
            get_byte(cursor_pos + 4)
        } else {
            None
        };
        let b5 = if bytes_available >= 6 {
            get_byte(cursor_pos + 5)
        } else {
            None
        };
        let b6 = if bytes_available >= 7 {
            get_byte(cursor_pos + 6)
        } else {
            None
        };
        let b7 = if bytes_available >= 8 {
            get_byte(cursor_pos + 7)
        } else {
            None
        };

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
        let (int64, uint64, float64) =
            if let (Some(b1), Some(b2), Some(b3), Some(b4), Some(b5), Some(b6), Some(b7)) =
                (b1, b2, b3, b4, b5, b6, b7)
            {
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

/// Buffer rows above/below visible area to prevent flickering during scroll
const BUFFER_ROWS: usize = 10;
/// Additional margin for partial rows and layout differences
const RENDER_MARGIN: usize = 4;

/// Result of visible range calculation
pub struct VisibleRange {
    pub render_start: usize,
    pub render_end: usize,
    pub visible_rows: usize, // Actual number of rows visible in viewport (without buffer)
    pub buffer_before: usize, // Actual buffer rows before first_visible_row
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
    anchor_first_row: Option<usize>,
) -> VisibleRange {
    if total_rows == 0 {
        return VisibleRange {
            render_start: 0,
            render_end: 0,
            visible_rows: 20,
            buffer_before: 0,
        };
    }

    // Use f64 for calculations to avoid precision loss
    let content_height_f64: f64 = f64::from(content_height).max(row_height);

    // Calculate number of visible rows (actual rows that fit in viewport)
    let visible_row_count = (content_height_f64 / row_height).floor() as usize;
    let visible_row_count = visible_row_count.max(5); // Minimum rows for rendering

    // Use anchor_first_row if provided (bypasses f32 precision loss for large files),
    // otherwise compute from scroll offset
    let first_visible_row = if let Some(anchor) = anchor_first_row {
        anchor
    } else {
        let scroll_offset_f64: f64 = scroll_offset.into();
        let actual_total_height = total_rows as f64 * row_height;

        if actual_total_height > MAX_VIRTUAL_HEIGHT {
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
        }
    };

    // Clamp first_visible_row to ensure we can render full viewport at the end
    let max_first_row = total_rows.saturating_sub(visible_row_count);
    let first_visible_row = first_visible_row.min(max_first_row);

    // For ratio-based virtual scrolling, limit buffer rows so that
    // top_spacer = |scroll_offset| - buffer_before * row_height >= 0
    // This ensures first_visible_row is positioned at the viewport top.
    let actual_total_height = total_rows as f64 * row_height;
    let max_buffer = if actual_total_height > MAX_VIRTUAL_HEIGHT {
        let scroll_abs: f64 = (-f64::from(f32::from(scroll_offset))).max(0.0);
        (scroll_abs / row_height).floor() as usize
    } else {
        BUFFER_ROWS
    };
    let buffer = BUFFER_ROWS.min(max_buffer).min(first_visible_row);

    let render_start = first_visible_row - buffer;
    let buffer_before = buffer;
    let render_end =
        (first_visible_row + visible_row_count + BUFFER_ROWS + RENDER_MARGIN).min(total_rows);

    VisibleRange {
        render_start,
        render_end,
        visible_rows: visible_row_count,
        buffer_before,
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

/// Scroll result containing both pixel offset and logical first visible row.
/// The logical row is needed because for very large files, the f32 pixel offset
/// loses precision during the row→offset→row round-trip conversion.
pub struct ScrollResult {
    pub offset: Pixels,
    pub target_first_row: usize,
}

/// Calculate the scroll offset needed to make a row visible
/// Returns Some(ScrollResult) if scrolling is needed, None if the row is already visible
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
) -> Option<ScrollResult> {
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

    // Clamp target_row same as calculate_scroll_offset does internally
    let max_target_row = total_rows.saturating_sub(visible_rows);
    let clamped_target = target_row.min(max_target_row);

    Some(ScrollResult {
        offset: calculate_scroll_offset(target_row, visible_rows, total_rows, row_height),
        target_first_row: clamped_target,
    })
}

/// Calculate spacer heights for virtual scrolling
/// Uses capped virtual height to avoid f32 precision issues
///
/// For large files using ratio-based scrolling, the top spacer is computed so that
/// first_visible_row appears at the viewport top. The key equation:
///   top_spacer + buffer_before * row_height = |scroll_offset|
///
/// Arguments:
/// - render_start: First row index to render
/// - render_end: Last row index to render (exclusive)
/// - total_rows: Total number of rows in the document
/// - row_height: Height of each row in pixels (font_size + margin)
/// - scroll_offset: Current scroll offset (for ratio-based spacer consistency)
/// - buffer_before: Number of buffer rows before first_visible_row
pub fn calculate_spacer_heights(
    render_start: usize,
    render_end: usize,
    total_rows: usize,
    row_height: f64,
    scroll_offset: Pixels,
    buffer_before: usize,
) -> (Pixels, Pixels) {
    if total_rows == 0 {
        return (px(0.0), px(0.0));
    }

    let actual_total_height = total_rows as f64 * row_height;

    if actual_total_height > MAX_VIRTUAL_HEIGHT {
        // Position first_visible_row at viewport top:
        // top_spacer + buffer_before * row_height = |scroll_offset|
        let scroll_abs: f64 = (-f64::from(f32::from(scroll_offset))).max(0.0);
        let top_height = (scroll_abs - buffer_before as f64 * row_height).max(0.0) as f32;

        // Bottom spacer: remaining space after top spacer and rendered rows
        let rendered_height = (render_end - render_start) as f64 * row_height;
        let bottom_height =
            (MAX_VIRTUAL_HEIGHT - top_height as f64 - rendered_height).max(0.0) as f32;

        (px(top_height), px(bottom_height))
    } else {
        // Direct calculation for smaller files
        let top_height = (render_start as f64 * row_height) as f32;
        let bottom_height = ((total_rows - render_end) as f64 * row_height) as f32;

        (px(top_height), px(bottom_height))
    }
}

/// Parameters for hit-testing mouse coordinates to byte positions.
///
/// This struct captures all layout parameters needed to convert mouse (x, y)
/// coordinates into a byte offset within the document. Extracted as a pure
/// function to enable unit testing independent of the rendering system.
pub struct HitTestLayout {
    pub outer_padding: f32,
    pub header_height: f32,
    pub content_top_padding: f32,
    /// Positive scroll offset (already negated from the raw negative value)
    pub scroll_offset: f32,
    pub row_height: f32,
    pub char_width: f32,
    pub bytes_per_row: usize,
    pub doc_len: usize,
    /// Number of hex characters for address display (from `address_chars()`)
    pub addr_chars: usize,
}

impl HitTestLayout {
    /// Bookmark indicator width: 8px dot + 4px margin (or 12px spacer)
    const BOOKMARK_INDICATOR: f32 = 12.0;
    /// gap_4 = 16px
    const GAP_4: f32 = 16.0;
    /// gap_1 = 4px between hex bytes
    const GAP_1: f32 = 4.0;

    /// X coordinate where the hex column starts
    fn hex_start_x(&self) -> f32 {
        let address_width = Self::BOOKMARK_INDICATOR + self.char_width * self.addr_chars as f32;
        self.outer_padding + address_width + Self::GAP_4
    }

    /// X coordinate where the ASCII column starts
    fn ascii_start_x(&self) -> f32 {
        let hex_start = self.hex_start_x();
        // Hex column width: each byte is 2 chars wide, with gap_1 between bytes (not after last)
        let hex_column_width = self.bytes_per_row as f32 * self.char_width * 2.0
            + (self.bytes_per_row.saturating_sub(1)) as f32 * Self::GAP_1;
        hex_start + hex_column_width + Self::GAP_4
    }

    /// Convert mouse Y coordinate to row index.
    ///
    /// The result is unbounded on the upper end — callers are responsible
    /// for clamping to the valid row range when converting to byte offsets.
    pub fn row_from_mouse_y(&self, mouse_y: f32) -> usize {
        let content_top =
            self.outer_padding + self.header_height + self.content_top_padding;
        let relative_y = mouse_y - content_top + self.scroll_offset;
        if relative_y < 0.0 {
            0
        } else {
            (relative_y / self.row_height) as usize
        }
    }

    /// Convert mouse X coordinate to byte-in-row index for Hex pane.
    ///
    /// Returns a value in `0..bytes_per_row` (clamped).
    pub fn byte_in_row_from_mouse_x_hex(&self, mouse_x: f32) -> usize {
        let hex_start = self.hex_start_x();
        let hex_byte_width = self.char_width * 2.0 + Self::GAP_1;
        if mouse_x < hex_start {
            0
        } else {
            let col = ((mouse_x - hex_start) / hex_byte_width) as usize;
            col.min(self.bytes_per_row.saturating_sub(1))
        }
    }

    /// Convert mouse X coordinate to byte-in-row index for ASCII pane.
    ///
    /// Returns a value in `0..bytes_per_row` (clamped).
    pub fn byte_in_row_from_mouse_x_ascii(&self, mouse_x: f32) -> usize {
        let ascii_start = self.ascii_start_x();
        if mouse_x < ascii_start {
            0
        } else {
            let col = ((mouse_x - ascii_start) / self.char_width) as usize;
            col.min(self.bytes_per_row.saturating_sub(1))
        }
    }

    /// Convert mouse coordinates to a byte position (main entry point).
    ///
    /// The returned value is clamped to `0..doc_len`.
    pub fn byte_position_from_mouse(&self, mouse_x: f32, mouse_y: f32, pane: EditPane) -> usize {
        if self.doc_len == 0 {
            return 0;
        }
        let row = self.row_from_mouse_y(mouse_y);
        let row_start = row * self.bytes_per_row;
        let byte_in_row = match pane {
            EditPane::Hex => self.byte_in_row_from_mouse_x_hex(mouse_x),
            EditPane::Ascii => self.byte_in_row_from_mouse_x_ascii(mouse_x),
            EditPane::Bitmap => 0,
        };
        (row_start + byte_in_row).min(self.doc_len.saturating_sub(1))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Create a HitTestLayout with typical defaults for testing.
    /// char_width=10, row_height=30, bytes_per_row=16, doc_len=256, addr_chars=8
    fn default_layout() -> HitTestLayout {
        HitTestLayout {
            outer_padding: 16.0,
            header_height: 50.0,
            content_top_padding: 16.0,
            scroll_offset: 0.0,
            row_height: 30.0,
            char_width: 10.0,
            bytes_per_row: 16,
            doc_len: 256,
            addr_chars: 8,
        }
    }

    // -- row_from_mouse_y --

    #[test]
    fn row_from_mouse_y_first_row() {
        let layout = default_layout();
        // content_top = 16 + 50 + 16 = 82
        // mouse_y = 82 => relative_y = 0 => row 0
        assert_eq!(layout.row_from_mouse_y(82.0), 0);
    }

    #[test]
    fn row_from_mouse_y_second_row() {
        let layout = default_layout();
        // mouse_y = 82 + 30 = 112 => relative_y = 30 => row 1
        assert_eq!(layout.row_from_mouse_y(112.0), 1);
    }

    #[test]
    fn row_from_mouse_y_negative_clamps_to_zero() {
        let layout = default_layout();
        // mouse_y = 0 => relative_y = -82 => clamped to row 0
        assert_eq!(layout.row_from_mouse_y(0.0), 0);
    }

    #[test]
    fn row_from_mouse_y_with_scroll_offset() {
        let mut layout = default_layout();
        layout.scroll_offset = 60.0; // scrolled down 2 rows
        // mouse_y = 82 => relative_y = 0 + 60 = 60 => row 2
        assert_eq!(layout.row_from_mouse_y(82.0), 2);
    }

    // -- byte_in_row_from_mouse_x_hex --

    #[test]
    fn hex_first_byte() {
        let layout = default_layout();
        // hex_start = 16 + (12 + 10*8) + 16 = 16 + 92 + 16 = 124
        let hex_start = 124.0;
        assert_eq!(layout.byte_in_row_from_mouse_x_hex(hex_start), 0);
    }

    #[test]
    fn hex_second_byte() {
        let layout = default_layout();
        // hex_byte_width = 10*2 + 4 = 24
        let hex_start = 124.0;
        assert_eq!(layout.byte_in_row_from_mouse_x_hex(hex_start + 24.0), 1);
    }

    #[test]
    fn hex_left_of_column_clamps_to_zero() {
        let layout = default_layout();
        assert_eq!(layout.byte_in_row_from_mouse_x_hex(0.0), 0);
    }

    #[test]
    fn hex_right_overflow_clamps_to_last_byte() {
        let layout = default_layout();
        assert_eq!(layout.byte_in_row_from_mouse_x_hex(9999.0), 15);
    }

    // -- byte_in_row_from_mouse_x_ascii --

    #[test]
    fn ascii_first_byte() {
        let layout = default_layout();
        // hex_start = 124
        // hex_column_width = 16*10*2 + 15*4 = 320 + 60 = 380
        // ascii_start = 124 + 380 + 16 = 520
        let ascii_start = 520.0;
        assert_eq!(layout.byte_in_row_from_mouse_x_ascii(ascii_start), 0);
    }

    #[test]
    fn ascii_second_byte() {
        let layout = default_layout();
        let ascii_start = 520.0;
        assert_eq!(layout.byte_in_row_from_mouse_x_ascii(ascii_start + 10.0), 1);
    }

    #[test]
    fn ascii_left_of_column_clamps_to_zero() {
        let layout = default_layout();
        assert_eq!(layout.byte_in_row_from_mouse_x_ascii(0.0), 0);
    }

    #[test]
    fn ascii_right_overflow_clamps_to_last_byte() {
        let layout = default_layout();
        assert_eq!(layout.byte_in_row_from_mouse_x_ascii(9999.0), 15);
    }

    // -- byte_position_from_mouse (integration) --

    #[test]
    fn byte_position_hex_first_byte() {
        let layout = default_layout();
        // row 0, byte 0 => offset 0
        assert_eq!(layout.byte_position_from_mouse(124.0, 82.0, EditPane::Hex), 0);
    }

    #[test]
    fn byte_position_hex_second_row_third_byte() {
        let layout = default_layout();
        // row 1 (y=112), byte 2 (x = 124 + 24*2 = 172) => offset 16+2 = 18
        assert_eq!(layout.byte_position_from_mouse(172.0, 112.0, EditPane::Hex), 18);
    }

    #[test]
    fn byte_position_ascii_pane() {
        let layout = default_layout();
        // row 0, byte 0
        assert_eq!(layout.byte_position_from_mouse(520.0, 82.0, EditPane::Ascii), 0);
        // row 0, byte 5 (x = 520 + 10*5 = 570)
        assert_eq!(layout.byte_position_from_mouse(570.0, 82.0, EditPane::Ascii), 5);
    }

    #[test]
    fn byte_position_clamps_to_doc_end() {
        let mut layout = default_layout();
        layout.doc_len = 20; // only 20 bytes
        // row 10, byte 0 => would be 160, clamped to 19
        assert_eq!(layout.byte_position_from_mouse(124.0, 382.0, EditPane::Hex), 19);
    }

    #[test]
    fn byte_position_empty_doc_returns_zero() {
        let mut layout = default_layout();
        layout.doc_len = 0;
        assert_eq!(layout.byte_position_from_mouse(200.0, 200.0, EditPane::Hex), 0);
    }
}
