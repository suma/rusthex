// PatternNode: evaluation output tree representing decoded binary regions

use std::sync::Arc;

use crate::parser::ast::{BuiltinType, Endianness};

/// A node in the evaluation result tree.
/// Each node represents a decoded region in the binary data.
#[derive(Debug, Clone)]
pub struct PatternNode {
    /// Display name of this pattern
    pub name: String,
    /// Type name (e.g., "u32", "Header", "Color")
    pub type_name: String,
    /// Byte offset in the binary data
    pub offset: u64,
    /// Size in bytes
    pub size: u64,
    /// Decoded value
    pub value: PatternValue,
    /// Child nodes (struct members, array elements, etc.)
    pub children: Vec<PatternNode>,
    /// Attributes (color, comment, hidden, etc.)
    pub attributes: PatternAttributes,
}

/// The decoded value of a pattern node
#[derive(Debug, Clone, PartialEq)]
pub enum PatternValue {
    Unsigned(u128),
    Signed(i128),
    Float(f64),
    Bool(bool),
    Char(char),
    String(String),
    /// Enum variant: value + variant name
    Enum {
        value: u128,
        name: String,
    },
    /// Array (children contain elements)
    Array,
    /// Struct (children contain members)
    Struct,
    /// Union (children contain members)
    Union,
    /// Bitfield (children contain fields)
    Bitfield,
    /// Pointer (contains target address)
    Pointer {
        address: u64,
    },
    /// Padding bytes
    Padding(u64),
    /// Bulk data for large fixed-size builtin arrays.
    /// Children are not materialized — element access decodes from this buffer.
    BulkData {
        data: Arc<Vec<u8>>,
        elem_type: BuiltinType,
        endian: Endianness,
    },
    /// Lazy struct array: elements are read on-demand when indexed.
    /// Only for fixed-size Named/Template types where type_size() succeeds.
    LazyStructArray {
        type_name: String,
        elem_size: u64,
        count: u64,
        endian: Endianness,
    },
}

/// Attributes that can be applied to pattern nodes
#[derive(Debug, Clone, Default)]
pub struct PatternAttributes {
    /// Display color in hex (e.g., "FF0000")
    pub color: Option<String>,
    /// Comment text
    pub comment: Option<String>,
    /// Whether this node is hidden from display
    pub hidden: bool,
    /// Custom display name
    pub display_name: Option<String>,
    /// Format string for value display
    pub format: Option<String>,
    /// Whether this node is sealed (children not expanded)
    pub sealed: bool,
    /// Whether struct members are inlined into parent
    pub inline: bool,
    /// Member does not advance the struct offset (overlaps next member)
    pub no_unique_address: bool,
}

impl PatternNode {
    pub fn new(
        name: impl Into<String>,
        type_name: impl Into<String>,
        offset: u64,
        size: u64,
        value: PatternValue,
    ) -> Self {
        Self {
            name: name.into(),
            type_name: type_name.into(),
            offset,
            size,
            value,
            children: Vec::new(),
            attributes: PatternAttributes::default(),
        }
    }

    /// Add a child node
    pub fn with_child(mut self, child: PatternNode) -> Self {
        self.children.push(child);
        self
    }

    /// Set color attribute
    pub fn with_color(mut self, color: String) -> Self {
        self.attributes.color = Some(color);
        self
    }

    /// Set comment attribute
    pub fn with_comment(mut self, comment: String) -> Self {
        self.attributes.comment = Some(comment);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pattern_node_creation() {
        let node = PatternNode::new("magic", "u32", 0, 4, PatternValue::Unsigned(0x474E5089));
        assert_eq!(node.name, "magic");
        assert_eq!(node.type_name, "u32");
        assert_eq!(node.offset, 0);
        assert_eq!(node.size, 4);
        assert_eq!(node.value, PatternValue::Unsigned(0x474E5089));
    }

    #[test]
    fn test_pattern_node_with_children() {
        let node = PatternNode::new("header", "Header", 0, 8, PatternValue::Struct)
            .with_child(PatternNode::new(
                "magic",
                "u32",
                0,
                4,
                PatternValue::Unsigned(0x89),
            ))
            .with_child(PatternNode::new(
                "version",
                "u16",
                4,
                2,
                PatternValue::Unsigned(1),
            ));
        assert_eq!(node.children.len(), 2);
        assert_eq!(node.children[0].name, "magic");
        assert_eq!(node.children[1].name, "version");
    }

    #[test]
    fn test_pattern_node_attributes() {
        let node = PatternNode::new("field", "u8", 0, 1, PatternValue::Unsigned(0))
            .with_color("FF0000".to_string())
            .with_comment("Important field".to_string());
        assert_eq!(node.attributes.color, Some("FF0000".to_string()));
        assert_eq!(node.attributes.comment, Some("Important field".to_string()));
    }
}
