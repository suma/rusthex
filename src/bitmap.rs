//! Bitmap visualization module for the hex editor
//!
//! This module provides functionality to visualize binary data as a bitmap image,
//! which can help identify patterns, structures, and anomalies in binary files.

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
