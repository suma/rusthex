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

/// Parameters used to determine if bitmap cache is valid
#[derive(Clone, PartialEq)]
pub(crate) struct BitmapCacheParams {
    pub scroll_start: usize,
    pub display_height: usize,
    pub bitmap_width: usize,
    pub pixel_size: u32,
    pub color_mode: BitmapColorMode,
    pub cursor_position: usize,
    pub doc_len: usize,
}

use crate::HexEditor;

impl HexEditor {
    /// Update bitmap image cache if parameters changed
    pub(crate) fn update_bitmap_cache(&mut self) {
        let doc_len = self.tab().document.len();
        let bitmap_width_pixels = self.bitmap_width;
        let bitmap_height = (doc_len + bitmap_width_pixels - 1) / bitmap_width_pixels;

        // Calculate pixel size
        let scrollbar_width = 12.0;
        let bitmap_area_width = self.bitmap_panel_width - 16.0 - scrollbar_width - 4.0;
        let pixel_size = (bitmap_area_width / bitmap_width_pixels as f32).max(1.0).floor();

        // Calculate canvas and display height
        let viewport_bounds = self.tab().scroll_handle.bounds();
        let bitmap_panel_height = f32::from(viewport_bounds.size.height);
        let panel_overhead = self.cached_line_height_sm
            + self.cached_line_height_xs * 2.0
            + 8.0 * 2.0
            + 8.0 * 3.0;
        let canvas_height = (bitmap_panel_height - panel_overhead).max(10.0 * pixel_size);
        let display_height = ((canvas_height / pixel_size) as usize).min(bitmap_height);

        // Calculate scroll position
        let bitmap_scroll_offset = self.bitmap_scroll_handle.offset();
        let bitmap_scroll_y: f32 = (-f32::from(bitmap_scroll_offset.y)).max(0.0);
        let bitmap_scroll_start = (bitmap_scroll_y / pixel_size) as usize;
        let bitmap_scroll_start = bitmap_scroll_start.min(bitmap_height.saturating_sub(display_height));

        let cursor_pos = self.tab().cursor_position;

        let current_params = BitmapCacheParams {
            scroll_start: bitmap_scroll_start,
            display_height,
            bitmap_width: bitmap_width_pixels,
            pixel_size: pixel_size as u32,
            color_mode: self.bitmap_color_mode,
            cursor_position: cursor_pos,
            doc_len,
        };

        // Check if cache is valid
        let cache_valid = self.cached_bitmap_params.as_ref()
            .map(|p| *p == current_params)
            .unwrap_or(false);

        if !cache_valid {
            // Create new bitmap image
            let doc = &self.tab().document;
            let new_image = create_bitmap_image(
                |offset| doc.get_byte(offset),
                doc_len,
                bitmap_scroll_start,
                display_height,
                bitmap_width_pixels,
                pixel_size,
                self.bitmap_color_mode,
                cursor_pos,
            );
            self.cached_bitmap_image = Some(new_image);
            self.cached_bitmap_params = Some(current_params);
        }
    }

    /// Toggle bitmap visualization
    pub(crate) fn toggle_bitmap(&mut self) {
        self.bitmap_visible = !self.bitmap_visible;
        if self.bitmap_visible {
            self.save_message = Some(format!(
                "Bitmap: {} ({}px wide)",
                self.bitmap_color_mode.label(),
                self.bitmap_width
            ));
        } else {
            self.save_message = Some("Bitmap view closed".to_string());
        }
    }

    /// Cycle bitmap color mode
    pub(crate) fn cycle_bitmap_color_mode(&mut self) {
        self.bitmap_color_mode = self.bitmap_color_mode.next();
        self.save_message = Some(format!("Bitmap mode: {}", self.bitmap_color_mode.label()));
    }

    /// Increase bitmap width
    pub(crate) fn increase_bitmap_width(&mut self) {
        self.bitmap_width = next_width(self.bitmap_width);
        self.save_message = Some(format!("Bitmap width: {}px", self.bitmap_width));
    }

    /// Decrease bitmap width
    pub(crate) fn decrease_bitmap_width(&mut self) {
        self.bitmap_width = prev_width(self.bitmap_width);
        self.save_message = Some(format!("Bitmap width: {}px", self.bitmap_width));
    }
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
