// Integration tests for the pattern language engine

use pattern_lang::{PatternEngine, PatternNode, PatternValue, SliceDataSource};

fn run(source: &str, data: &[u8]) -> Vec<PatternNode> {
    let engine = PatternEngine::new();
    let ds = SliceDataSource::new(data);
    engine.run(source, &ds).expect("engine.run failed")
}

// ========== PNG Header Test ==========

#[test]
fn test_png_header() {
    let source = r#"
        struct PNGHeader {
            u32 magic;
            u32 reserved;
        };
        be PNGHeader header @ 0x00;
    "#;
    // PNG signature starts with 0x89504E47
    let data: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A];
    let results = run(source, data);

    assert_eq!(results.len(), 1);
    assert_eq!(results[0].name, "header");
    assert_eq!(results[0].type_name, "PNGHeader");
    assert_eq!(results[0].value, PatternValue::Struct);
    assert_eq!(results[0].children.len(), 2);
    assert_eq!(
        results[0].children[0].value,
        PatternValue::Unsigned(0x89504E47)
    );
    assert_eq!(
        results[0].children[1].value,
        PatternValue::Unsigned(0x0D0A1A0A)
    );
}

// ========== Simple Binary Format Test ==========

#[test]
fn test_simple_binary_format() {
    let source = r#"
        enum FileType : u8 {
            Text = 0,
            Binary = 1,
            Archive = 2
        };

        struct FileHeader {
            u32 magic;
            u8 version;
            FileType type;
            u16 entry_count;
        };

        FileHeader header @ 0x00;
    "#;
    let data: &[u8] = &[
        0x46, 0x4F, 0x4F, 0x00, // magic = "FOO\0" = 0x004F4F46 (LE)
        0x01, // version = 1
        0x02, // type = Archive
        0x03, 0x00, // entry_count = 3 (LE)
    ];
    let results = run(source, data);

    assert_eq!(results.len(), 1);
    let header = &results[0];
    assert_eq!(header.children.len(), 4);
    assert_eq!(header.children[1].value, PatternValue::Unsigned(1)); // version
    assert_eq!(
        header.children[2].value,
        PatternValue::Enum {
            value: 2,
            name: "Archive".to_string()
        }
    );
    assert_eq!(header.children[3].value, PatternValue::Unsigned(3)); // entry_count
}

// ========== Array of Structs Test ==========

#[test]
fn test_array_of_structs() {
    let source = r#"
        struct Entry {
            u16 id;
            u16 value;
        };

        Entry entries[3] @ 0x00;
    "#;
    let data: &[u8] = &[
        0x01, 0x00, 0x0A, 0x00, // entry 0: id=1, value=10
        0x02, 0x00, 0x14, 0x00, // entry 1: id=2, value=20
        0x03, 0x00, 0x1E, 0x00, // entry 2: id=3, value=30
    ];
    let results = run(source, data);

    assert_eq!(results[0].value, PatternValue::Array);
    assert_eq!(results[0].children.len(), 3);

    assert_eq!(
        results[0].children[0].children[0].value,
        PatternValue::Unsigned(1)
    ); // id
    assert_eq!(
        results[0].children[0].children[1].value,
        PatternValue::Unsigned(10)
    ); // value
    assert_eq!(
        results[0].children[2].children[0].value,
        PatternValue::Unsigned(3)
    ); // id
    assert_eq!(
        results[0].children[2].children[1].value,
        PatternValue::Unsigned(30)
    ); // value
}

// ========== Nested Struct with Mixed Endianness ==========

#[test]
fn test_mixed_endianness() {
    let source = r#"
        struct MixedHeader {
            be u32 magic;
            le u16 version;
            be u16 flags;
        };
        MixedHeader header @ 0x00;
    "#;
    let data: &[u8] = &[
        0x89, 0x50, 0x4E, 0x47, // magic (BE) = 0x89504E47
        0x01, 0x00, // version (LE) = 1
        0x00, 0x03, // flags (BE) = 3
    ];
    let results = run(source, data);

    assert_eq!(
        results[0].children[0].value,
        PatternValue::Unsigned(0x89504E47)
    );
    assert_eq!(results[0].children[1].value, PatternValue::Unsigned(1));
    assert_eq!(results[0].children[2].value, PatternValue::Unsigned(3));
}

// ========== Union Test ==========

#[test]
fn test_union_type() {
    let source = r#"
        union IntOrFloat {
            u32 as_int;
            float as_float;
        };
        IntOrFloat val @ 0x00;
    "#;
    // 1.0f32 in little-endian
    let data: &[u8] = &[0x00, 0x00, 0x80, 0x3F];
    let results = run(source, data);

    assert_eq!(results[0].value, PatternValue::Union);
    assert_eq!(results[0].size, 4);
    assert_eq!(results[0].children.len(), 2);
    assert_eq!(
        results[0].children[0].value,
        PatternValue::Unsigned(0x3F800000)
    );
    match &results[0].children[1].value {
        PatternValue::Float(v) => assert!((v - 1.0).abs() < 0.001),
        other => panic!("expected Float, got {:?}", other),
    }
}

// ========== Conditional Fields Test ==========

#[test]
fn test_conditional_fields() {
    let source = r#"
        struct Header {
            u8 version;
            u8 flags;
        };
        Header h @ 0x00;

        if (h.version == 2) {
            u16 extra @ 0x02;
        }
    "#;
    // version=2, so extra field should appear
    let data: &[u8] = &[0x02, 0xFF, 0x42, 0x00];
    let results = run(source, data);

    // h (Header) + extra (u16)
    assert_eq!(results.len(), 2);
    assert_eq!(results[0].name, "h");
    assert_eq!(results[0].children[0].value, PatternValue::Unsigned(2)); // version
    assert_eq!(results[1].name, "extra");
    assert_eq!(results[1].value, PatternValue::Unsigned(0x0042)); // 0x42, 0x00 LE
}

// ========== Using / Type Alias Test ==========

#[test]
fn test_type_alias() {
    let source = r#"
        using DWORD = u32;
        using WORD = u16;

        struct Header {
            DWORD magic;
            WORD version;
            WORD flags;
        };

        Header header @ 0x00;
    "#;
    let data: &[u8] = &[0x89, 0x50, 0x4E, 0x47, 0x01, 0x00, 0x03, 0x00];
    let results = run(source, data);

    assert_eq!(results[0].children.len(), 3);
    assert_eq!(
        results[0].children[0].value,
        PatternValue::Unsigned(0x474E5089)
    );
}

// ========== Function Call Test ==========

#[test]
fn test_function_in_pattern() {
    let source = r#"
        fn compute_count(u32 x) -> u32 {
            return x + 1;
        };

        u32 raw @ 0x00;
    "#;
    let data: &[u8] = &[0x05, 0x00, 0x00, 0x00]; // 5 in LE
    let results = run(source, data);

    assert_eq!(results[0].value, PatternValue::Unsigned(5));
}

// ========== Multiple Placements Test ==========

#[test]
fn test_multiple_placements() {
    let source = r#"
        u32 first @ 0x00;
        u32 second @ 0x04;
        u8 byte @ 0x02;
    "#;
    let data: &[u8] = &[0x01, 0x00, 0x42, 0x00, 0x02, 0x00, 0x00, 0x00];
    let results = run(source, data);

    assert_eq!(results.len(), 3);
    assert_eq!(results[0].value, PatternValue::Unsigned(0x00420001)); // first
    assert_eq!(results[0].offset, 0);
    assert_eq!(results[1].value, PatternValue::Unsigned(2)); // second
    assert_eq!(results[1].offset, 4);
    assert_eq!(results[2].value, PatternValue::Unsigned(0x42)); // byte
    assert_eq!(results[2].offset, 2);
}

// ========== Signed Types Test ==========

#[test]
fn test_signed_types() {
    let source = r#"
        struct SignedData {
            s8 a;
            s16 b;
            s32 c;
        };
        SignedData d @ 0x00;
    "#;
    let data: &[u8] = &[
        0xFF, // a = -1
        0xFE, 0xFF, // b = -2 (LE)
        0xFD, 0xFF, 0xFF, 0xFF, // c = -3 (LE)
    ];
    let results = run(source, data);

    assert_eq!(results[0].children[0].value, PatternValue::Signed(-1));
    assert_eq!(results[0].children[1].value, PatternValue::Signed(-2));
    assert_eq!(results[0].children[2].value, PatternValue::Signed(-3));
}

// ========== Double Type Test ==========

#[test]
fn test_double_type() {
    let source = "double val @ 0x00;";
    let data = std::f64::consts::PI.to_le_bytes();
    let results = run(source, &data);

    match &results[0].value {
        PatternValue::Float(v) => assert!((v - std::f64::consts::PI).abs() < 1e-10),
        other => panic!("expected Float, got {:?}", other),
    }
}

// ========== Pattern Engine API Test ==========

#[test]
fn test_engine_parse_and_evaluate_separately() {
    let engine = PatternEngine::new();
    let source = "u32 x @ 0x00;";
    let data = [0x42, 0x00, 0x00, 0x00];

    let ast = engine.parse(source).unwrap();
    let ds = SliceDataSource::new(&data);
    let results = engine.evaluate(&ast, &ds).unwrap();

    assert_eq!(results[0].value, PatternValue::Unsigned(0x42));
}

// ========== Offset and Size Verification ==========

#[test]
fn test_struct_offset_size() {
    let source = r#"
        struct Record {
            u8 tag;
            u32 payload;
            u8 checksum;
        };
        Record r @ 0x10;
    "#;
    let mut data = vec![0u8; 0x16];
    data[0x10] = 0xAA; // tag
    data[0x11] = 0x01; // payload byte 0
    data[0x12] = 0x02; // payload byte 1
    data[0x13] = 0x03; // payload byte 2
    data[0x14] = 0x04; // payload byte 3
    data[0x15] = 0xBB; // checksum

    let results = run(source, &data);

    assert_eq!(results[0].offset, 0x10);
    assert_eq!(results[0].size, 6);
    assert_eq!(results[0].children[0].offset, 0x10);
    assert_eq!(results[0].children[0].size, 1);
    assert_eq!(results[0].children[1].offset, 0x11);
    assert_eq!(results[0].children[1].size, 4);
    assert_eq!(results[0].children[2].offset, 0x15);
    assert_eq!(results[0].children[2].size, 1);
    assert_eq!(results[0].children[2].value, PatternValue::Unsigned(0xBB));
}

// ========== pck.hexpat-like pattern test ==========

#[test]
fn test_pck_like_string_transform() {
    // Minimal reproduction of pck.hexpat transform chain:
    // SizedStringBase with char16 data, transform to get string value,
    // and accessing transformed value through ref parameter.
    let source = r#"
        namespace impl {
            fn format_string(ref auto s) {
                return s.data;
            };
        }

        struct SizedString16 {
            s32 size;
            char16 data[size];
        } [[sealed, transform("impl::format_string")]];

        struct Entry {
            s32 index;
            SizedString16 value;
        } [[sealed]];

        struct Table {
            s32 count;
            Entry entries[count];
        };

        fn table_contains(ref Table t, str needle) {
            for (s32 i = 0, i < t.count, i = i + 1) {
                str entry_val = t.entries[i].value;
                if (entry_val == needle)
                    return true;
            }
            return false;
        };

        be Table table @ 0x00;

        // Direct access test (no function call)
        str direct_val = table.entries[1].value;
        if (direct_val == "BC") {
            u8 marker_direct @ 0x1C;
        }

        bool found_bc = table_contains(table, "BC");
        bool found_zz = table_contains(table, "ZZ");

        // Use the results to place data (DataPlacement verifies the function ran)
        if (found_bc) {
            u8 marker_bc @ 0x1C;
        }
        if (found_zz) {
            u8 marker_zz @ 0x1C;
        }
    "#;
    // Build data: count=2, then two entries:
    //   Entry 0: index=0, SizedString16(size=2, data="AB")
    //   Entry 1: index=1, SizedString16(size=2, data="BC")
    let mut data = Vec::new();
    // count = 2 (big endian s32)
    data.extend_from_slice(&[0x00, 0x00, 0x00, 0x02]);
    // Entry 0: index=0
    data.extend_from_slice(&[0x00, 0x00, 0x00, 0x00]);
    // Entry 0: SizedString16: size=2
    data.extend_from_slice(&[0x00, 0x00, 0x00, 0x02]);
    // Entry 0: SizedString16: data = 'A', 'B' (char16 big endian)
    data.extend_from_slice(&[0x00, 0x41, 0x00, 0x42]);
    // Entry 1: index=1
    data.extend_from_slice(&[0x00, 0x00, 0x00, 0x01]);
    // Entry 1: SizedString16: size=2
    data.extend_from_slice(&[0x00, 0x00, 0x00, 0x02]);
    // Entry 1: SizedString16: data = 'B', 'C' (char16 big endian)
    data.extend_from_slice(&[0x00, 0x42, 0x00, 0x43]);
    // Extra byte for the `found` variable
    data.push(0x00);

    let engine = PatternEngine::new();
    let ds = SliceDataSource::new(&data);
    let results = engine.run(source, &ds).expect("engine.run failed");

    // Check the table structure
    let table_node = &results[0];
    assert_eq!(table_node.name, "table");
    let entries_node = &table_node.children[1]; // entries array
    assert_eq!(entries_node.name, "entries");

    // Verify that transformed struct nodes have their value updated in the node tree.
    // Before the fix, child_node.value stayed as PatternValue::Struct even after
    // [[transform("impl::format_string")]] converted it to a string.
    let entry0 = &entries_node.children[0];
    let entry0_value = &entry0.children[1]; // value member (SizedString16)
    assert_eq!(
        entry0_value.value,
        PatternValue::String("AB".to_string()),
        "SizedString16 node should have transformed String value, not Struct"
    );
    let entry1 = &entries_node.children[1];
    let entry1_value = &entry1.children[1]; // value member (SizedString16)
    assert_eq!(
        entry1_value.value,
        PatternValue::String("BC".to_string()),
        "SizedString16 node should have transformed String value, not Struct"
    );

    // If table_contains works correctly, found_bc=true should place marker_bc,
    // and found_zz=false should NOT place marker_zz
    let has_marker_bc = results.iter().any(|n| n.name == "marker_bc");
    let has_marker_zz = results.iter().any(|n| n.name == "marker_zz");
    assert!(
        has_marker_bc,
        "table_contains should find 'BC' in the table"
    );
    assert!(
        !has_marker_zz,
        "table_contains should NOT find 'ZZ' in the table"
    );
}
