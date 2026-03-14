//! Magic-byte file signature detection.

/// Confidence level of a signature match.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Confidence {
    /// Magic bytes fully matched (4+ bytes).
    Definite,
    /// Short or ambiguous magic (2–3 bytes).
    Likely,
}

/// A detected file signature within the data.
#[derive(Debug, Clone)]
pub struct SignatureMatch {
    /// Byte offset within the input where the signature was found.
    pub offset: usize,
    /// Human-readable format name (e.g. "PNG", "gzip").
    pub name: &'static str,
    /// MIME type (e.g. "image/png").
    pub mime: &'static str,
    /// Match confidence.
    pub confidence: Confidence,
}

struct MagicEntry {
    magic: &'static [u8],
    offset: usize,
    name: &'static str,
    mime: &'static str,
    confidence: Confidence,
}

const SIGNATURES: &[MagicEntry] = &[
    // Images
    MagicEntry {
        magic: b"\x89PNG\r\n\x1a\n",
        offset: 0,
        name: "PNG",
        mime: "image/png",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\xff\xd8\xff",
        offset: 0,
        name: "JPEG",
        mime: "image/jpeg",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"GIF87a",
        offset: 0,
        name: "GIF87a",
        mime: "image/gif",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"GIF89a",
        offset: 0,
        name: "GIF89a",
        mime: "image/gif",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"BM",
        offset: 0,
        name: "BMP",
        mime: "image/bmp",
        confidence: Confidence::Likely,
    },
    MagicEntry {
        magic: b"RIFF",
        offset: 0,
        name: "RIFF",
        mime: "application/octet-stream",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"WEBP",
        offset: 8,
        name: "WebP",
        mime: "image/webp",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\x00\x00\x01\x00",
        offset: 0,
        name: "ICO",
        mime: "image/x-icon",
        confidence: Confidence::Likely,
    },
    // Archives & compression
    MagicEntry {
        magic: b"PK\x03\x04",
        offset: 0,
        name: "ZIP/JAR/DOCX",
        mime: "application/zip",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\x1f\x8b",
        offset: 0,
        name: "gzip",
        mime: "application/gzip",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"BZh",
        offset: 0,
        name: "bzip2",
        mime: "application/x-bzip2",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\xfd7zXZ\x00",
        offset: 0,
        name: "XZ",
        mime: "application/x-xz",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"7z\xbc\xaf\x27\x1c",
        offset: 0,
        name: "7-Zip",
        mime: "application/x-7z-compressed",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\x28\xb5\x2f\xfd",
        offset: 0,
        name: "Zstandard",
        mime: "application/zstd",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"Rar!\x1a\x07",
        offset: 0,
        name: "RAR",
        mime: "application/vnd.rar",
        confidence: Confidence::Definite,
    },
    // Executables
    MagicEntry {
        magic: b"\x7fELF",
        offset: 0,
        name: "ELF",
        mime: "application/x-elf",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"MZ",
        offset: 0,
        name: "PE/MZ (Windows EXE/DLL)",
        mime: "application/vnd.microsoft.portable-executable",
        confidence: Confidence::Likely,
    },
    MagicEntry {
        magic: b"\xfe\xed\xfa\xce",
        offset: 0,
        name: "Mach-O (32-bit)",
        mime: "application/x-mach-binary",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\xfe\xed\xfa\xcf",
        offset: 0,
        name: "Mach-O (64-bit)",
        mime: "application/x-mach-binary",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\xcf\xfa\xed\xfe",
        offset: 0,
        name: "Mach-O (64-bit, reversed)",
        mime: "application/x-mach-binary",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\xca\xfe\xba\xbe",
        offset: 0,
        name: "Mach-O Universal / Java Class",
        mime: "application/x-mach-binary",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\xbe\xba\xfe\xca",
        offset: 0,
        name: "Mach-O Universal (reversed)",
        mime: "application/x-mach-binary",
        confidence: Confidence::Definite,
    },
    // Documents
    MagicEntry {
        magic: b"%PDF",
        offset: 0,
        name: "PDF",
        mime: "application/pdf",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\xd0\xcf\x11\xe0\xa1\xb1\x1a\xe1",
        offset: 0,
        name: "OLE2 (DOC/XLS/PPT)",
        mime: "application/x-ole-storage",
        confidence: Confidence::Definite,
    },
    // Audio/Video
    MagicEntry {
        magic: b"ID3",
        offset: 0,
        name: "MP3 (ID3)",
        mime: "audio/mpeg",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"\xff\xfb",
        offset: 0,
        name: "MP3 frame",
        mime: "audio/mpeg",
        confidence: Confidence::Likely,
    },
    MagicEntry {
        magic: b"OggS",
        offset: 0,
        name: "Ogg",
        mime: "audio/ogg",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"fLaC",
        offset: 0,
        name: "FLAC",
        mime: "audio/flac",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"ftyp",
        offset: 4,
        name: "MP4/MOV",
        mime: "video/mp4",
        confidence: Confidence::Definite,
    },
    // Databases & data
    MagicEntry {
        magic: b"SQLite format 3\x00",
        offset: 0,
        name: "SQLite",
        mime: "application/vnd.sqlite3",
        confidence: Confidence::Definite,
    },
    // Fonts
    MagicEntry {
        magic: b"\x00\x01\x00\x00",
        offset: 0,
        name: "TrueType font",
        mime: "font/ttf",
        confidence: Confidence::Likely,
    },
    MagicEntry {
        magic: b"OTTO",
        offset: 0,
        name: "OpenType font",
        mime: "font/otf",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"wOFF",
        offset: 0,
        name: "WOFF",
        mime: "font/woff",
        confidence: Confidence::Definite,
    },
    MagicEntry {
        magic: b"wOF2",
        offset: 0,
        name: "WOFF2",
        mime: "font/woff2",
        confidence: Confidence::Definite,
    },
    // Disk images & firmware
    MagicEntry {
        magic: b"\xeb\x3c\x90",
        offset: 0,
        name: "FAT boot sector",
        mime: "application/x-fat",
        confidence: Confidence::Likely,
    },
    // WebAssembly
    MagicEntry {
        magic: b"\x00asm",
        offset: 0,
        name: "WebAssembly",
        mime: "application/wasm",
        confidence: Confidence::Definite,
    },
    // Protobuf / FlatBuffers (less common at offset 0, but useful)
    MagicEntry {
        magic: b"\x0a\x09IHDR",
        offset: 0,
        name: "PNG (truncated header)",
        mime: "image/png",
        confidence: Confidence::Likely,
    },
];

/// Scan the data for known file signatures.
///
/// Checks magic bytes at their expected offsets. Returns all matches
/// found, ordered by offset.
pub fn detect_signature(data: &[u8]) -> Vec<SignatureMatch> {
    let mut matches = Vec::new();

    for entry in SIGNATURES {
        let start = entry.offset;
        let end = start + entry.magic.len();
        if end <= data.len() && data[start..end] == *entry.magic {
            matches.push(SignatureMatch {
                offset: start,
                name: entry.name,
                mime: entry.mime,
                confidence: entry.confidence,
            });
        }
    }

    matches.sort_by_key(|m| m.offset);
    matches
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn detect_png() {
        let mut data = vec![0u8; 64];
        data[..8].copy_from_slice(b"\x89PNG\r\n\x1a\n");
        let sigs = detect_signature(&data);
        assert_eq!(sigs.len(), 1);
        assert_eq!(sigs[0].name, "PNG");
        assert_eq!(sigs[0].mime, "image/png");
        assert_eq!(sigs[0].confidence, Confidence::Definite);
    }

    #[test]
    fn detect_elf() {
        let mut data = vec![0u8; 16];
        data[..4].copy_from_slice(b"\x7fELF");
        let sigs = detect_signature(&data);
        assert!(sigs.iter().any(|s| s.name == "ELF"));
    }

    #[test]
    fn detect_zip() {
        let mut data = vec![0u8; 32];
        data[..4].copy_from_slice(b"PK\x03\x04");
        let sigs = detect_signature(&data);
        assert!(sigs.iter().any(|s| s.name == "ZIP/JAR/DOCX"));
    }

    #[test]
    fn detect_pdf() {
        let data = b"%PDF-1.4 rest of content...";
        let sigs = detect_signature(data);
        assert_eq!(sigs.len(), 1);
        assert_eq!(sigs[0].name, "PDF");
    }

    #[test]
    fn detect_gzip() {
        let data = [0x1f, 0x8b, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00];
        let sigs = detect_signature(&data);
        assert!(sigs.iter().any(|s| s.name == "gzip"));
    }

    #[test]
    fn detect_macho_64() {
        let data = [0xcf, 0xfa, 0xed, 0xfe, 0x07, 0x00, 0x00, 0x01];
        let sigs = detect_signature(&data);
        assert!(sigs.iter().any(|s| s.name.contains("Mach-O")));
    }

    #[test]
    fn detect_mp4_at_offset() {
        // MP4 has "ftyp" at offset 4
        let mut data = vec![0u8; 16];
        data[4..8].copy_from_slice(b"ftyp");
        let sigs = detect_signature(&data);
        assert!(sigs.iter().any(|s| s.name == "MP4/MOV"));
        assert_eq!(sigs[0].offset, 4);
    }

    #[test]
    fn detect_webp() {
        // RIFF at 0, WEBP at 8
        let mut data = vec![0u8; 16];
        data[..4].copy_from_slice(b"RIFF");
        data[8..12].copy_from_slice(b"WEBP");
        let sigs = detect_signature(&data);
        assert!(sigs.iter().any(|s| s.name == "RIFF"));
        assert!(sigs.iter().any(|s| s.name == "WebP"));
    }

    #[test]
    fn no_match() {
        let data = [0x01, 0x02, 0x03, 0x04];
        let sigs = detect_signature(&data);
        assert!(sigs.is_empty());
    }

    #[test]
    fn empty_data() {
        let sigs = detect_signature(&[]);
        assert!(sigs.is_empty());
    }

    #[test]
    fn too_short_for_magic() {
        // PNG magic is 8 bytes, data is only 4
        let data = b"\x89PNG";
        let sigs = detect_signature(data);
        assert!(!sigs.iter().any(|s| s.name == "PNG"));
    }

    #[test]
    fn detect_sqlite() {
        let mut data = vec![0u8; 32];
        data[..16].copy_from_slice(b"SQLite format 3\x00");
        let sigs = detect_signature(&data);
        assert!(sigs.iter().any(|s| s.name == "SQLite"));
    }

    #[test]
    fn detect_wasm() {
        let data = [0x00, b'a', b's', b'm', 0x01, 0x00, 0x00, 0x00];
        let sigs = detect_signature(&data);
        assert!(sigs.iter().any(|s| s.name == "WebAssembly"));
    }
}
