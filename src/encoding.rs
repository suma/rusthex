//! Text encoding support for ASCII column display
//!
//! This module provides functions to decode bytes for display in the ASCII column
//! using various text encodings (ASCII, Latin-1, UTF-8, Shift-JIS, EUC-JP).

use crate::ui::TextEncoding;
use encoding_rs::{EUC_JP, SHIFT_JIS, UTF_8};

/// Display character type for decoded bytes
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DisplayChar {
    /// Successfully decoded character (shown at first byte of multi-byte sequence)
    Char(char),
    /// Continuation byte of multi-byte sequence (shown as '·')
    Continuation,
    /// Invalid byte sequence (shown as '.')
    Invalid,
    /// Non-printable control character (shown as '.')
    NonPrintable,
}

impl DisplayChar {
    /// Convert to display character
    pub fn to_char(&self) -> char {
        match self {
            DisplayChar::Char(c) => *c,
            DisplayChar::Continuation => '·',
            DisplayChar::Invalid => '.',
            DisplayChar::NonPrintable => '.',
        }
    }

    /// Check if this is a continuation marker
    pub fn is_continuation(&self) -> bool {
        matches!(self, DisplayChar::Continuation)
    }
}

/// Decode bytes for display according to specified encoding
///
/// Returns a vector of DisplayChar for each byte position.
/// For multi-byte encodings:
/// - First byte of a character shows the decoded character
/// - Continuation bytes show '·' marker
/// - Invalid sequences show '.'
pub fn decode_for_display(data: &[u8], encoding: TextEncoding) -> Vec<DisplayChar> {
    if data.is_empty() {
        return Vec::new();
    }

    match encoding {
        TextEncoding::Ascii => decode_ascii(data),
        TextEncoding::Latin1 => decode_latin1(data),
        TextEncoding::Utf8 => decode_utf8(data),
        TextEncoding::ShiftJis => decode_with_encoding_rs(data, SHIFT_JIS),
        TextEncoding::EucJp => decode_with_encoding_rs(data, EUC_JP),
        TextEncoding::Utf16Be => decode_utf16(data, true),
        TextEncoding::Utf16Le => decode_utf16(data, false),
        TextEncoding::Utf32Be => decode_utf32(data, true),
        TextEncoding::Utf32Le => decode_utf32(data, false),
    }
}

/// Decode as pure ASCII (0x20-0x7E only)
fn decode_ascii(data: &[u8]) -> Vec<DisplayChar> {
    data.iter()
        .map(|&b| {
            if b >= 0x20 && b <= 0x7E {
                DisplayChar::Char(b as char)
            } else {
                DisplayChar::NonPrintable
            }
        })
        .collect()
}

/// Decode as Latin-1 (ISO-8859-1)
fn decode_latin1(data: &[u8]) -> Vec<DisplayChar> {
    data.iter()
        .map(|&b| {
            if b >= 0x20 && b <= 0x7E {
                // Printable ASCII
                DisplayChar::Char(b as char)
            } else if b >= 0xA0 {
                // Latin-1 supplement (printable)
                DisplayChar::Char(b as char)
            } else {
                // Control characters
                DisplayChar::NonPrintable
            }
        })
        .collect()
}

/// Decode as UTF-8
fn decode_utf8(data: &[u8]) -> Vec<DisplayChar> {
    let mut result = vec![DisplayChar::Invalid; data.len()];
    let mut i = 0;

    while i < data.len() {
        let byte = data[i];

        // Determine UTF-8 sequence length
        let seq_len = if byte & 0x80 == 0 {
            1 // ASCII
        } else if byte & 0xE0 == 0xC0 {
            2 // 2-byte sequence
        } else if byte & 0xF0 == 0xE0 {
            3 // 3-byte sequence
        } else if byte & 0xF8 == 0xF0 {
            4 // 4-byte sequence
        } else {
            // Invalid start byte
            result[i] = DisplayChar::Invalid;
            i += 1;
            continue;
        };

        // Check if we have enough bytes
        if i + seq_len > data.len() {
            result[i] = DisplayChar::Invalid;
            i += 1;
            continue;
        }

        // Validate continuation bytes
        let mut valid = true;
        for j in 1..seq_len {
            if data[i + j] & 0xC0 != 0x80 {
                valid = false;
                break;
            }
        }

        if !valid {
            result[i] = DisplayChar::Invalid;
            i += 1;
            continue;
        }

        // Try to decode the sequence
        if let Ok(s) = std::str::from_utf8(&data[i..i + seq_len]) {
            if let Some(c) = s.chars().next() {
                // Check if printable (all control characters shown as dot)
                if c.is_control() {
                    result[i] = DisplayChar::NonPrintable;
                } else {
                    result[i] = DisplayChar::Char(c);
                }
                // Mark continuation bytes
                for j in 1..seq_len {
                    result[i + j] = DisplayChar::Continuation;
                }
            }
        } else {
            result[i] = DisplayChar::Invalid;
        }

        i += seq_len;
    }

    result
}

/// Decode using encoding_rs for Shift-JIS and EUC-JP
fn decode_with_encoding_rs(
    data: &[u8],
    encoding: &'static encoding_rs::Encoding,
) -> Vec<DisplayChar> {
    let mut result = vec![DisplayChar::Invalid; data.len()];

    // Use encoding_rs to decode and track byte positions
    let (decoded, _, had_errors) = encoding.decode(data);

    if had_errors {
        // Try byte-by-byte for better granularity
        decode_encoding_rs_granular(data, encoding, &mut result);
    } else {
        // Map decoded characters back to byte positions
        map_decoded_to_bytes(data, &decoded, encoding, &mut result);
    }

    result
}

/// Granular decoding for better error handling
fn decode_encoding_rs_granular(
    data: &[u8],
    encoding: &'static encoding_rs::Encoding,
    result: &mut [DisplayChar],
) {
    let mut i = 0;

    while i < data.len() {
        // Try to decode from current position with increasing lengths
        let mut decoded = false;

        for len in 1..=4.min(data.len() - i) {
            let slice = &data[i..i + len];
            let (s, _, had_errors) = encoding.decode(slice);

            if !had_errors && !s.is_empty() {
                if let Some(c) = s.chars().next() {
                    if c.is_control() {
                        result[i] = DisplayChar::NonPrintable;
                    } else {
                        result[i] = DisplayChar::Char(c);
                    }
                    for j in 1..len {
                        result[i + j] = DisplayChar::Continuation;
                    }
                    i += len;
                    decoded = true;
                    break;
                }
            }
        }

        if !decoded {
            result[i] = DisplayChar::Invalid;
            i += 1;
        }
    }
}

/// Map decoded string back to byte positions
fn map_decoded_to_bytes(
    data: &[u8],
    decoded: &str,
    encoding: &'static encoding_rs::Encoding,
    result: &mut [DisplayChar],
) {
    // For UTF-8 compatible encodings, we can use a simpler approach
    if encoding == UTF_8 {
        decode_utf8_into(data, result);
        return;
    }

    // For other encodings, try character-by-character
    let mut byte_pos = 0;

    for c in decoded.chars() {
        if byte_pos >= data.len() {
            break;
        }

        // Estimate byte length for this character
        let char_bytes = if c.is_ascii() { 1 } else { 2 };
        let actual_bytes = char_bytes.min(data.len() - byte_pos);

        if c.is_control() {
            result[byte_pos] = DisplayChar::NonPrintable;
        } else {
            result[byte_pos] = DisplayChar::Char(c);
        }

        for j in 1..actual_bytes {
            if byte_pos + j < result.len() {
                result[byte_pos + j] = DisplayChar::Continuation;
            }
        }

        byte_pos += actual_bytes;
    }
}

/// Helper to decode UTF-8 into existing result slice
fn decode_utf8_into(data: &[u8], result: &mut [DisplayChar]) {
    let decoded = decode_utf8(data);
    for (i, dc) in decoded.into_iter().enumerate() {
        if i < result.len() {
            result[i] = dc;
        }
    }
}

/// Decode as UTF-16 (BE or LE)
fn decode_utf16(data: &[u8], is_big_endian: bool) -> Vec<DisplayChar> {
    let mut result = vec![DisplayChar::Invalid; data.len()];
    let mut i = 0;

    while i < data.len() {
        // Need at least 2 bytes for a code unit
        if i + 1 >= data.len() {
            result[i] = DisplayChar::Invalid;
            break;
        }

        let code_unit = if is_big_endian {
            u16::from_be_bytes([data[i], data[i + 1]])
        } else {
            u16::from_le_bytes([data[i], data[i + 1]])
        };

        // Check for surrogate pair
        if (0xD800..=0xDBFF).contains(&code_unit) {
            // High surrogate - need low surrogate
            if i + 3 >= data.len() {
                // Not enough data for low surrogate
                result[i] = DisplayChar::Invalid;
                result[i + 1] = DisplayChar::Continuation;
                i += 2;
                continue;
            }

            let low = if is_big_endian {
                u16::from_be_bytes([data[i + 2], data[i + 3]])
            } else {
                u16::from_le_bytes([data[i + 2], data[i + 3]])
            };

            if !(0xDC00..=0xDFFF).contains(&low) {
                // Orphan high surrogate
                result[i] = DisplayChar::Invalid;
                result[i + 1] = DisplayChar::Continuation;
                i += 2;
                continue;
            }

            // Decode surrogate pair
            let cp = 0x10000 + ((code_unit as u32 - 0xD800) << 10) + (low as u32 - 0xDC00);
            if let Some(c) = char::from_u32(cp) {
                if c.is_control() {
                    result[i] = DisplayChar::NonPrintable;
                } else {
                    result[i] = DisplayChar::Char(c);
                }
                result[i + 1] = DisplayChar::Continuation;
                result[i + 2] = DisplayChar::Continuation;
                result[i + 3] = DisplayChar::Continuation;
                i += 4;
            } else {
                result[i] = DisplayChar::Invalid;
                result[i + 1] = DisplayChar::Continuation;
                i += 2;
            }
        } else if (0xDC00..=0xDFFF).contains(&code_unit) {
            // Orphan low surrogate
            result[i] = DisplayChar::Invalid;
            result[i + 1] = DisplayChar::Continuation;
            i += 2;
        } else {
            // BMP character
            if let Some(c) = char::from_u32(code_unit as u32) {
                if c.is_control() {
                    result[i] = DisplayChar::NonPrintable;
                } else {
                    result[i] = DisplayChar::Char(c);
                }
                result[i + 1] = DisplayChar::Continuation;
            } else {
                result[i] = DisplayChar::Invalid;
                result[i + 1] = DisplayChar::Continuation;
            }
            i += 2;
        }
    }

    result
}

/// Decode as UTF-32 (BE or LE)
fn decode_utf32(data: &[u8], is_big_endian: bool) -> Vec<DisplayChar> {
    let mut result = vec![DisplayChar::Invalid; data.len()];
    let mut i = 0;

    while i < data.len() {
        // Need at least 4 bytes
        if i + 3 >= data.len() {
            // Mark remaining bytes as Invalid
            for j in i..data.len() {
                result[j] = DisplayChar::Invalid;
            }
            break;
        }

        let cp = if is_big_endian {
            u32::from_be_bytes([data[i], data[i + 1], data[i + 2], data[i + 3]])
        } else {
            u32::from_le_bytes([data[i], data[i + 1], data[i + 2], data[i + 3]])
        };

        if let Some(c) = char::from_u32(cp) {
            if c.is_control() {
                result[i] = DisplayChar::NonPrintable;
            } else {
                result[i] = DisplayChar::Char(c);
            }
        } else {
            result[i] = DisplayChar::Invalid;
        }

        result[i + 1] = DisplayChar::Continuation;
        result[i + 2] = DisplayChar::Continuation;
        result[i + 3] = DisplayChar::Continuation;
        i += 4;
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ascii_decode() {
        let data = b"Hello";
        let result = decode_for_display(data, TextEncoding::Ascii);
        assert_eq!(result.len(), 5);
        assert_eq!(result[0].to_char(), 'H');
        assert_eq!(result[4].to_char(), 'o');
    }

    #[test]
    fn test_ascii_non_printable() {
        let data = &[0x00, 0x1F, 0x7F];
        let result = decode_for_display(data, TextEncoding::Ascii);
        assert_eq!(result[0].to_char(), '.');
        assert_eq!(result[1].to_char(), '.');
        assert_eq!(result[2].to_char(), '.');
    }

    #[test]
    fn test_utf8_multibyte() {
        // "あ" in UTF-8 is E3 81 82
        let data = &[0xE3, 0x81, 0x82];
        let result = decode_for_display(data, TextEncoding::Utf8);
        assert_eq!(result.len(), 3);
        assert_eq!(result[0].to_char(), 'あ');
        assert!(result[1].is_continuation());
        assert!(result[2].is_continuation());
    }

    #[test]
    fn test_utf8_invalid() {
        // Invalid UTF-8 sequence
        let data = &[0xFF, 0xFE];
        let result = decode_for_display(data, TextEncoding::Utf8);
        assert_eq!(result[0].to_char(), '.');
        assert_eq!(result[1].to_char(), '.');
    }

    #[test]
    fn test_latin1() {
        // Latin-1 extended characters
        let data = &[0xC0, 0xE9]; // À, é
        let result = decode_for_display(data, TextEncoding::Latin1);
        assert_eq!(result[0].to_char(), 'À');
        assert_eq!(result[1].to_char(), 'é');
    }

    #[test]
    fn test_utf16be_bmp() {
        // "あ" (U+3042) in UTF-16BE: 30 42
        let data = &[0x30, 0x42];
        let result = decode_for_display(data, TextEncoding::Utf16Be);
        assert_eq!(result.len(), 2);
        assert_eq!(result[0].to_char(), 'あ');
        assert!(result[1].is_continuation());
    }

    #[test]
    fn test_utf16le_surrogate() {
        // U+1F600 (😀) in UTF-16LE: surrogate pair D83D DE00
        // LE bytes: 3D D8 00 DE
        let data = &[0x3D, 0xD8, 0x00, 0xDE];
        let result = decode_for_display(data, TextEncoding::Utf16Le);
        assert_eq!(result.len(), 4);
        assert_eq!(result[0].to_char(), '😀');
        assert!(result[1].is_continuation());
        assert!(result[2].is_continuation());
        assert!(result[3].is_continuation());
    }

    #[test]
    fn test_utf32be_decode() {
        // "あ" (U+3042) in UTF-32BE: 00 00 30 42
        let data = &[0x00, 0x00, 0x30, 0x42];
        let result = decode_for_display(data, TextEncoding::Utf32Be);
        assert_eq!(result.len(), 4);
        assert_eq!(result[0].to_char(), 'あ');
        assert!(result[1].is_continuation());
        assert!(result[2].is_continuation());
        assert!(result[3].is_continuation());
    }

    #[test]
    fn test_utf32le_decode() {
        // "A" (U+0041) in UTF-32LE: 41 00 00 00
        let data = &[0x41, 0x00, 0x00, 0x00];
        let result = decode_for_display(data, TextEncoding::Utf32Le);
        assert_eq!(result.len(), 4);
        assert_eq!(result[0].to_char(), 'A');
        assert!(result[1].is_continuation());
        assert!(result[2].is_continuation());
        assert!(result[3].is_continuation());
    }
}
