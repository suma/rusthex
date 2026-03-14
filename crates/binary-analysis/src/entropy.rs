//! Shannon entropy analysis.

use std::fmt;

/// Data classification based on entropy value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DataClass {
    /// Entropy < 1.0 — null fill, padding, uniform data
    Uniform,
    /// Entropy 1.0–3.5 — ASCII text, config files, source code
    PlainText,
    /// Entropy 3.5–5.0 — structured binary (headers, metadata)
    StructuredBinary,
    /// Entropy 5.0–6.5 — native executables, multimedia
    Executable,
    /// Entropy 6.5–7.5 — compressed data (gzip, zstd, etc.)
    Compressed,
    /// Entropy 7.5–8.0 — encrypted or cryptographically random
    EncryptedOrRandom,
}

impl DataClass {
    fn from_entropy(e: f64) -> Self {
        if e < 1.0 {
            Self::Uniform
        } else if e < 3.5 {
            Self::PlainText
        } else if e < 5.0 {
            Self::StructuredBinary
        } else if e < 6.5 {
            Self::Executable
        } else if e < 7.5 {
            Self::Compressed
        } else {
            Self::EncryptedOrRandom
        }
    }
}

impl fmt::Display for DataClass {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Uniform => write!(f, "Very low entropy — likely uniform/empty data"),
            Self::PlainText => write!(f, "Low entropy — likely plain text or structured data"),
            Self::StructuredBinary => write!(f, "Medium-low entropy — structured binary data"),
            Self::Executable => write!(f, "Medium entropy — executable code or multimedia"),
            Self::Compressed => write!(f, "High entropy — possibly compressed data"),
            Self::EncryptedOrRandom => {
                write!(f, "Very high entropy — likely encrypted or compressed data")
            }
        }
    }
}

/// Entropy analysis result.
#[derive(Debug, Clone)]
pub struct EntropyReport {
    /// Shannon entropy in bits per byte (0.0–8.0).
    pub entropy: f64,
    /// Number of distinct byte values present (0–256).
    pub unique_bytes: u16,
    /// Classification based on entropy value.
    pub data_class: DataClass,
}

/// Calculate Shannon entropy and classify the data.
pub fn entropy(data: &[u8]) -> EntropyReport {
    if data.is_empty() {
        return EntropyReport {
            entropy: 0.0,
            unique_bytes: 0,
            data_class: DataClass::Uniform,
        };
    }

    let mut freq = [0u64; 256];
    for &b in data {
        freq[b as usize] += 1;
    }

    let unique_bytes = freq.iter().filter(|&&c| c > 0).count() as u16;

    let len = data.len() as f64;
    let mut e = 0.0;
    for &count in &freq {
        if count > 0 {
            let p = count as f64 / len;
            e -= p * p.log2();
        }
    }

    EntropyReport {
        entropy: e,
        unique_bytes,
        data_class: DataClass::from_entropy(e),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_data() {
        let r = entropy(&[]);
        assert_eq!(r.entropy, 0.0);
        assert_eq!(r.unique_bytes, 0);
        assert_eq!(r.data_class, DataClass::Uniform);
    }

    #[test]
    fn uniform_single_value() {
        let data = vec![0xAA; 1024];
        let r = entropy(&data);
        assert!((r.entropy - 0.0).abs() < 0.001);
        assert_eq!(r.unique_bytes, 1);
        assert_eq!(r.data_class, DataClass::Uniform);
    }

    #[test]
    fn two_equal_values() {
        let mut data = Vec::new();
        for _ in 0..512 {
            data.push(0x00);
            data.push(0xFF);
        }
        let r = entropy(&data);
        assert!((r.entropy - 1.0).abs() < 0.001);
        assert_eq!(r.unique_bytes, 2);
        assert_eq!(r.data_class, DataClass::PlainText);
    }

    #[test]
    fn all_256_values_equal() {
        let mut data = Vec::new();
        for _ in 0..100 {
            for b in 0u8..=255 {
                data.push(b);
            }
        }
        let r = entropy(&data);
        assert!((r.entropy - 8.0).abs() < 0.001);
        assert_eq!(r.unique_bytes, 256);
        assert_eq!(r.data_class, DataClass::EncryptedOrRandom);
    }

    #[test]
    fn plain_text() {
        let data = b"Hello, World! This is a test of entropy calculation.";
        let r = entropy(data);
        assert!(r.entropy > 2.0 && r.entropy < 5.0, "entropy={}", r.entropy);
    }

    #[test]
    fn data_class_display() {
        assert!(format!("{}", DataClass::Compressed).contains("compressed"));
        assert!(format!("{}", DataClass::PlainText).contains("plain text"));
    }
}
