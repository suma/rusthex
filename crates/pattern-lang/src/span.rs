// Source position tracking for diagnostics

/// Unique identifier for a source file
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(pub u32);

/// A span representing a range in source code
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub start: u32,
    pub end: u32,
    pub source_id: SourceId,
}

impl Span {
    pub fn new(start: u32, end: u32, source_id: SourceId) -> Self {
        Self {
            start,
            end,
            source_id,
        }
    }

    /// Create a dummy span for testing
    pub fn dummy() -> Self {
        Self {
            start: 0,
            end: 0,
            source_id: SourceId(0),
        }
    }

    /// Merge two spans into one that covers both
    pub fn merge(self, other: Span) -> Span {
        debug_assert_eq!(self.source_id, other.source_id);
        Span {
            start: self.start.min(other.start),
            end: self.end.max(other.end),
            source_id: self.source_id,
        }
    }
}

/// Tracks source files for error reporting
#[derive(Debug, Default)]
pub struct SourceMap {
    sources: Vec<Source>,
}

#[derive(Debug)]
struct Source {
    name: String,
    content: String,
    /// Byte offsets of line starts
    line_starts: Vec<u32>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            sources: Vec::new(),
        }
    }

    /// Add a source file and return its ID
    pub fn add(&mut self, name: String, content: String) -> SourceId {
        let line_starts = compute_line_starts(&content);
        let id = SourceId(self.sources.len() as u32);
        self.sources.push(Source {
            name,
            content,
            line_starts,
        });
        id
    }

    /// Get the source name for a given ID
    pub fn name(&self, id: SourceId) -> &str {
        &self.sources[id.0 as usize].name
    }

    /// Get the source content for a given ID
    pub fn content(&self, id: SourceId) -> &str {
        &self.sources[id.0 as usize].content
    }

    /// Convert a byte offset to (line, column), both 1-based
    pub fn line_col(&self, id: SourceId, offset: u32) -> (u32, u32) {
        let source = &self.sources[id.0 as usize];
        let line = match source.line_starts.binary_search(&offset) {
            Ok(i) => i,
            Err(i) => i.saturating_sub(1),
        };
        let col = offset - source.line_starts[line];
        (line as u32 + 1, col + 1)
    }
}

/// Convert a byte offset to (line, col), both 1-based.
/// Standalone function that doesn't require SourceMap.
pub fn offset_to_line_col(source: &str, offset: u32) -> (u32, u32) {
    let offset = offset as usize;
    let mut line = 1u32;
    let mut col = 1u32;
    for (i, b) in source.bytes().enumerate() {
        if i >= offset {
            break;
        }
        if b == b'\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }
    (line, col)
}

fn compute_line_starts(content: &str) -> Vec<u32> {
    let mut starts = vec![0u32];
    for (i, b) in content.bytes().enumerate() {
        if b == b'\n' {
            starts.push(i as u32 + 1);
        }
    }
    starts
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_span_merge() {
        let a = Span::new(5, 10, SourceId(0));
        let b = Span::new(8, 20, SourceId(0));
        let merged = a.merge(b);
        assert_eq!(merged.start, 5);
        assert_eq!(merged.end, 20);
    }

    #[test]
    fn test_source_map_line_col() {
        let mut map = SourceMap::new();
        let id = map.add("test".into(), "abc\ndef\nghi".into());
        assert_eq!(map.line_col(id, 0), (1, 1)); // 'a'
        assert_eq!(map.line_col(id, 3), (1, 4)); // '\n'
        assert_eq!(map.line_col(id, 4), (2, 1)); // 'd'
        assert_eq!(map.line_col(id, 8), (3, 1)); // 'g'
    }

    #[test]
    fn test_source_map_name_content() {
        let mut map = SourceMap::new();
        let id = map.add("hello.hexpat".into(), "content here".into());
        assert_eq!(map.name(id), "hello.hexpat");
        assert_eq!(map.content(id), "content here");
    }

    #[test]
    fn test_offset_to_line_col() {
        let src = "abc\ndef\nghi";
        assert_eq!(offset_to_line_col(src, 0), (1, 1)); // 'a'
        assert_eq!(offset_to_line_col(src, 3), (1, 4)); // '\n'
        assert_eq!(offset_to_line_col(src, 4), (2, 1)); // 'd'
        assert_eq!(offset_to_line_col(src, 8), (3, 1)); // 'g'
        assert_eq!(offset_to_line_col(src, 10), (3, 3)); // 'i'
    }

    #[test]
    fn test_offset_to_line_col_empty() {
        assert_eq!(offset_to_line_col("", 0), (1, 1));
    }
}
