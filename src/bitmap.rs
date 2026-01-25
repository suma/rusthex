//! Bitmap visualization module for the hex editor
//!
//! This module provides functionality to visualize binary data as a bitmap image,
//! which can help identify patterns, structures, and anomalies in binary files.

use std::sync::Arc;
use gpui::RenderImage;
use image::{ImageBuffer, Rgba, Frame, Delay};
use smallvec::smallvec;
use std::time::Duration;

/// Color mode for bitmap visualization
#[derive(Debug, Clone, Copy, PartialEq, Default)]
pub enum BitmapColorMode {
    /// Grayscale: 0x00 = black, 0xFF = white
    #[default]
    Grayscale,
    /// Heatmap: low values = blue, high values = red
    Heatmap,
    /// Category: different colors for different byte ranges
    Category,
}

impl BitmapColorMode {
    /// Get display label for this color mode
    pub fn label(&self) -> &'static str {
        match self {
            BitmapColorMode::Grayscale => "Grayscale",
            BitmapColorMode::Heatmap => "Heatmap",
            BitmapColorMode::Category => "Category",
        }
    }

    /// Cycle to next color mode
    pub fn next(&self) -> BitmapColorMode {
        match self {
            BitmapColorMode::Grayscale => BitmapColorMode::Heatmap,
            BitmapColorMode::Heatmap => BitmapColorMode::Category,
            BitmapColorMode::Category => BitmapColorMode::Grayscale,
        }
    }

    /// Get viewport indicator color for this color mode
    /// Returns RGB color value as u32
    pub fn indicator_color(&self) -> u32 {
        match self {
            BitmapColorMode::Grayscale => 0x00ff00,  // Green: high contrast against grayscale
            BitmapColorMode::Heatmap => 0xeeeeee,    // Light gray: visible against colorful gradient
            BitmapColorMode::Category => 0xeeeeee,   // Light gray: visible against colorful background
        }
    }

    /// Convert byte value to RGB color based on color mode
    pub fn byte_to_color(&self, byte: u8) -> (u8, u8, u8) {
        match self {
            BitmapColorMode::Grayscale => (byte, byte, byte),
            BitmapColorMode::Heatmap => {
                // Blue (cold) -> Cyan -> Green -> Yellow -> Red (hot)
                let v = byte as f32 / 255.0;
                if v < 0.25 {
                    let t = v * 4.0;
                    (0, (t * 255.0) as u8, 255)
                } else if v < 0.5 {
                    let t = (v - 0.25) * 4.0;
                    (0, 255, (255.0 * (1.0 - t)) as u8)
                } else if v < 0.75 {
                    let t = (v - 0.5) * 4.0;
                    ((t * 255.0) as u8, 255, 0)
                } else {
                    let t = (v - 0.75) * 4.0;
                    (255, (255.0 * (1.0 - t)) as u8, 0)
                }
            }
            BitmapColorMode::Category => {
                // Different colors for different byte ranges
                match byte {
                    0x00 => (0, 0, 0),            // Null: black
                    0x01..=0x1F => (128, 0, 128), // Control: purple
                    0x20 => (64, 64, 64),         // Space: dark gray
                    0x21..=0x2F => (255, 165, 0), // Punctuation: orange
                    0x30..=0x39 => (0, 255, 0),   // Digits: green
                    0x3A..=0x40 => (255, 165, 0), // Punctuation: orange
                    0x41..=0x5A => (0, 128, 255), // Uppercase: blue
                    0x5B..=0x60 => (255, 165, 0), // Punctuation: orange
                    0x61..=0x7A => (0, 200, 255), // Lowercase: light blue
                    0x7B..=0x7E => (255, 165, 0), // Punctuation: orange
                    0x7F => (128, 0, 128),        // DEL: purple
                    0x80..=0xFE => (255, 255, 0), // High bytes: yellow
                    0xFF => (255, 0, 0),          // 0xFF: red
                }
            }
        }
    }
}

/// Available bitmap widths
pub const BITMAP_WIDTHS: &[usize] = &[64, 128, 256, 512, 1024];

/// Default bitmap width
pub const DEFAULT_BITMAP_WIDTH: usize = 256;

/// Get the next bitmap width in the list
pub fn next_width(current: usize) -> usize {
    for (i, &w) in BITMAP_WIDTHS.iter().enumerate() {
        if w == current {
            return BITMAP_WIDTHS[(i + 1) % BITMAP_WIDTHS.len()];
        }
    }
    DEFAULT_BITMAP_WIDTH
}

/// Get the previous bitmap width in the list
pub fn prev_width(current: usize) -> usize {
    for (i, &w) in BITMAP_WIDTHS.iter().enumerate() {
        if w == current {
            return BITMAP_WIDTHS[(i + BITMAP_WIDTHS.len() - 1) % BITMAP_WIDTHS.len()];
        }
    }
    DEFAULT_BITMAP_WIDTH
}

/// Create a RenderImage from document bytes for the visible region
///
/// # Arguments
/// * `get_byte` - Function to get a byte at a given offset
/// * `doc_len` - Total document length
/// * `start_row` - Starting row in the bitmap
/// * `display_rows` - Number of rows to display
/// * `bitmap_width` - Width of the bitmap in pixels (data width)
/// * `pixel_size` - Size of each pixel in the output image
/// * `color_mode` - Color mode for rendering
/// * `cursor_position` - Current cursor position (for highlighting)
///
/// # Returns
/// Arc<RenderImage> containing the rendered bitmap at display size
pub fn create_bitmap_image<F>(
    get_byte: F,
    doc_len: usize,
    start_row: usize,
    display_rows: usize,
    bitmap_width: usize,
    pixel_size: f32,
    color_mode: BitmapColorMode,
    cursor_position: usize,
) -> Arc<RenderImage>
where
    F: Fn(usize) -> Option<u8>,
{
    let scale = pixel_size.max(1.0) as u32;
    let width = (bitmap_width as u32) * scale;
    let height = (display_rows as u32) * scale;

    // Create RGBA image buffer at display size
    let mut img_buffer: ImageBuffer<Rgba<u8>, Vec<u8>> = ImageBuffer::new(width, height);

    for row in 0..display_rows {
        let actual_row = start_row + row;
        let row_start = actual_row * bitmap_width;

        for col in 0..bitmap_width {
            let byte_offset = row_start + col;
            let (r, g, b) = if byte_offset < doc_len {
                if let Some(byte) = get_byte(byte_offset) {
                    color_mode.byte_to_color(byte)
                } else {
                    (0x1e, 0x1e, 0x1e) // Background color
                }
            } else {
                (0x1e, 0x1e, 0x1e) // Background color for empty area
            };

            // Add cursor highlight (red border effect)
            let (r, g, b) = if byte_offset == cursor_position {
                (255, 0, 0) // Red for cursor
            } else {
                (r, g, b)
            };

            // Fill scaled pixel area
            let px_x = (col as u32) * scale;
            let px_y = (row as u32) * scale;
            for dy in 0..scale {
                for dx in 0..scale {
                    img_buffer.put_pixel(px_x + dx, px_y + dy, Rgba([r, g, b, 255]));
                }
            }
        }
    }

    // Create Frame and RenderImage
    let frame = Frame::from_parts(img_buffer, 0, 0, Delay::from_saturating_duration(Duration::from_millis(0)));
    Arc::new(RenderImage::new(smallvec![frame]))
}
