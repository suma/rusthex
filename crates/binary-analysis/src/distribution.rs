//! Byte frequency distribution analysis.

/// Byte distribution statistics.
#[derive(Debug, Clone)]
pub struct ByteDistribution {
    /// Frequency count for each byte value 0x00–0xFF.
    pub histogram: [u32; 256],
    /// Ratio of 0x00 bytes (0.0–1.0).
    pub null_ratio: f64,
    /// Ratio of printable ASCII bytes 0x20–0x7E (0.0–1.0).
    pub printable_ratio: f64,
    /// Ratio of high bytes 0x80–0xFF (0.0–1.0).
    pub high_byte_ratio: f64,
}

/// Compute byte frequency distribution and derived ratios.
pub fn byte_distribution(data: &[u8]) -> ByteDistribution {
    let mut histogram = [0u32; 256];
    for &b in data {
        histogram[b as usize] += 1;
    }

    if data.is_empty() {
        return ByteDistribution {
            histogram,
            null_ratio: 0.0,
            printable_ratio: 0.0,
            high_byte_ratio: 0.0,
        };
    }

    let len = data.len() as f64;
    let null_count = histogram[0x00] as f64;
    let printable_count: f64 = histogram[0x20..=0x7E].iter().map(|&c| c as f64).sum();
    let high_count: f64 = histogram[0x80..=0xFF].iter().map(|&c| c as f64).sum();

    ByteDistribution {
        histogram,
        null_ratio: null_count / len,
        printable_ratio: printable_count / len,
        high_byte_ratio: high_count / len,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_data() {
        let d = byte_distribution(&[]);
        assert_eq!(d.null_ratio, 0.0);
        assert_eq!(d.printable_ratio, 0.0);
        assert_eq!(d.high_byte_ratio, 0.0);
    }

    #[test]
    fn all_nulls() {
        let data = vec![0x00; 100];
        let d = byte_distribution(&data);
        assert!((d.null_ratio - 1.0).abs() < 0.001);
        assert_eq!(d.printable_ratio, 0.0);
        assert_eq!(d.high_byte_ratio, 0.0);
        assert_eq!(d.histogram[0x00], 100);
    }

    #[test]
    fn pure_ascii_text() {
        let data = b"Hello World";
        let d = byte_distribution(data);
        assert_eq!(d.null_ratio, 0.0);
        assert!((d.printable_ratio - 1.0).abs() < 0.001);
        assert_eq!(d.high_byte_ratio, 0.0);
    }

    #[test]
    fn high_bytes() {
        let data: Vec<u8> = (0x80..=0xFF).collect();
        let d = byte_distribution(&data);
        assert!((d.high_byte_ratio - 1.0).abs() < 0.001);
        assert_eq!(d.printable_ratio, 0.0);
    }

    #[test]
    fn histogram_counts() {
        let data = vec![0x41, 0x41, 0x42, 0x42, 0x42];
        let d = byte_distribution(&data);
        assert_eq!(d.histogram[0x41], 2);
        assert_eq!(d.histogram[0x42], 3);
        assert_eq!(d.histogram[0x43], 0);
    }

    #[test]
    fn mixed_data() {
        // 2 nulls, 3 printable, 5 high bytes = 10 total
        let data: Vec<u8> = vec![0x00, 0x00, 0x41, 0x42, 0x43, 0x80, 0x90, 0xA0, 0xB0, 0xFF];
        let d = byte_distribution(&data);
        assert!((d.null_ratio - 0.2).abs() < 0.001);
        assert!((d.printable_ratio - 0.3).abs() < 0.001);
        assert!((d.high_byte_ratio - 0.5).abs() < 0.001);
    }
}
