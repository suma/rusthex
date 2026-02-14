// DataSource trait: abstraction over binary data access

use crate::error::EvalError;

/// Trait for accessing binary data during evaluation
pub trait DataSource {
    /// Read bytes from the given offset
    fn read_bytes(&self, offset: u64, size: u64) -> Result<Vec<u8>, EvalError>;

    /// Get the total size of the data
    fn size(&self) -> u64;
}

/// DataSource backed by a byte slice (useful for testing)
pub struct SliceDataSource<'a> {
    data: &'a [u8],
}

impl<'a> SliceDataSource<'a> {
    pub fn new(data: &'a [u8]) -> Self {
        Self { data }
    }
}

impl<'a> DataSource for SliceDataSource<'a> {
    fn read_bytes(&self, offset: u64, size: u64) -> Result<Vec<u8>, EvalError> {
        let start = offset as usize;
        let end = start.checked_add(size as usize).unwrap_or(usize::MAX);
        if end > self.data.len() {
            return Err(EvalError::read_oob(format!(
                "read out of bounds: offset={}, size={}, data_len={}",
                offset,
                size,
                self.data.len()
            )));
        }
        Ok(self.data[start..end].to_vec())
    }

    fn size(&self) -> u64 {
        self.data.len() as u64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_slice_data_source_read() {
        let data = vec![0x89, 0x50, 0x4E, 0x47];
        let ds = SliceDataSource::new(&data);
        assert_eq!(ds.read_bytes(0, 4).unwrap(), vec![0x89, 0x50, 0x4E, 0x47]);
        assert_eq!(ds.read_bytes(1, 2).unwrap(), vec![0x50, 0x4E]);
    }

    #[test]
    fn test_slice_data_source_out_of_bounds() {
        let data = vec![0x00, 0x01];
        let ds = SliceDataSource::new(&data);
        assert!(ds.read_bytes(0, 3).is_err());
    }

    #[test]
    fn test_slice_data_source_size() {
        let data = vec![0; 100];
        let ds = SliceDataSource::new(&data);
        assert_eq!(ds.size(), 100);
    }
}
