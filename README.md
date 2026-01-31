# RustHex

A modern hex editor built with Rust and [gpui](https://www.gpui.rs/), featuring GPU-accelerated rendering and efficient handling of large files.

## Features

### Core Functionality
- **3-Column Layout**: Address, Hex, and ASCII views synchronized in real-time
- **Dual Edit Panes**: Edit bytes in either Hex or ASCII pane (toggle with Tab)
- **Overwrite / Insert Mode**: Toggle with Insert key or Ctrl+Shift+I
  - **Overwrite (OVR)**: Replace existing bytes in-place (default)
  - **Insert (INS)**: Insert new bytes at cursor position, increasing file size
  - Backspace deletes the byte before cursor, Delete removes byte at cursor (Insert mode only)
  - Status bar shows current mode (OVR in white, INS in cyan)
- **Large File Support**: Memory-mapped files for efficient handling of files > 10MB
- **Virtual Scrolling**: Smooth performance even with gigabyte-sized files
- **Undo/Redo**: Full edit history with 1,000 operation limit

### Navigation & Selection
- **Keyboard Navigation**: Arrow keys, Page Up/Down, Home/End, Ctrl+Home/End
- **Mouse Support**: Click to position cursor, drag to select ranges
- **Selection**: Shift+Arrow keys or mouse drag to select byte ranges
- **Select All**: Ctrl+A / Cmd+A
- **Auto-scroll**: Cursor automatically stays visible during navigation

### Search Functionality
- **Dual Search Modes**:
  - ASCII text search
  - Hex value search (space-separated, e.g., "48 65 6C 6C 6F")
  - Wildcard support: `??` matches any byte (e.g., "48 ?? 6C 6C 6F")
- **SIMD-Accelerated Search**: Uses memchr for 10-50x faster exact pattern matching
- **Background Search**: Non-blocking search with cancellation support
- **Visual Indicators**:
  - All matches highlighted in yellow
  - Current match highlighted in orange
  - Scrollbar markers showing match positions
- **Navigation**: F3/Enter (next), Shift+F3 (previous)

### Data Inspector
- **View bytes as various types** (Ctrl+I to toggle)
- **Supported types**: Int8/16/32/64, UInt8/16/32/64, Float32/64
- **Endianness toggle**: Little/Big endian (Ctrl+E)

### Text Encoding Support
- **Multiple Encodings**: ASCII, Latin-1, UTF-8, Shift-JIS, EUC-JP
- **Display Only**: Encoding affects text display, editing remains byte-based
- **Quick Switch**: Ctrl+Shift+E to cycle through encodings, or click in status bar
- **Multi-byte Display**: First byte shows character, continuation bytes show `·`

### File Operations
- **Open Files**: Command-line, file dialog (Ctrl+O), or drag & drop
- **Save**: Ctrl+S / Cmd+S with confirmation dialog
- **Save As**: Ctrl+Shift+S / Cmd+Shift+S to save to new location
- **Unsaved Changes Indicator**: Visual feedback in title and status
- **Multiple File Tabs**: Open multiple files in tabs
  - Ctrl+T: New tab
  - Ctrl+W: Close current tab
  - Ctrl+Tab / Ctrl+Shift+Tab: Switch between tabs
  - Drag & drop tabs to reorder

### Bookmarks
- **Toggle Bookmark**: Ctrl+B to add/remove bookmark at cursor position
- **Bookmark Comments**: Add optional comments to bookmarks
  - Comment input bar appears when adding a bookmark
  - Edit existing comments with Ctrl+Shift+B
  - Comments displayed in status message when jumping to bookmarks
- **Navigate Bookmarks**: F2 (next), Shift+F2 (previous) with wraparound
- **Visual Indicators**:
  - Cyan markers for bookmarks without comments, green for bookmarks with comments
  - Underline on bookmarked bytes in Hex and ASCII columns
  - Scrollbar markers and address column dot indicators

### Compare Mode
- **Side-by-side Comparison**: Compare two files in split view (Ctrl+K)
- **Difference Highlighting**: Bytes that differ are highlighted in red
- **Synchronized Navigation**: Both panels scroll and navigate together

### Bitmap Visualization
- **Toggle Bitmap View**: Ctrl+M to show/hide bitmap panel
- **Color Modes** (press C to cycle):
  - Grayscale: 0x00 = black, 0xFF = white
  - Heatmap: Blue (low) to Red (high)
  - Category: Colors by byte type (null, control, digits, letters, etc.)
- **Width Adjustment**: +/- keys to change bitmap width (64, 128, 256, 512, 1024)
- **Cursor Tracking**: Current position highlighted in bitmap view

### Configuration
- **TOML Configuration File**:
  - macOS: `~/Library/Application Support/rusthex/config.toml`
  - Linux: `~/.config/rusthex/config.toml`
  - Windows: `%APPDATA%\rusthex\config.toml`
- **Customizable Settings**:
  - Font family (default: Monaco)
  - Font size
  - Bytes per row (default: 16)
  - Default endianness for data inspector

**Example config.toml:**
```toml
[display]
font_name = "Menlo"
font_size = 14.0
bytes_per_row = 16
```

## Keyboard Shortcuts

| Shortcut | Action |
|----------|--------|
| **Navigation** ||
| Arrow Keys | Move cursor |
| Page Up / Page Down | Move by page |
| Home / End | Go to start/end of row |
| Ctrl+Home / Ctrl+End | Go to start/end of file |
| Shift+Arrow / Shift+Page | Extend selection |
| Ctrl+A / Cmd+A | Select all |
| **Editing** ||
| Tab | Switch between Hex/ASCII pane |
| Insert / Ctrl+Shift+I | Toggle Overwrite/Insert mode |
| 0-9, A-F | Edit hex value (in Hex pane) |
| Printable chars | Edit ASCII value (in ASCII pane) |
| Backspace (Insert mode) | Delete byte before cursor |
| Delete (Insert mode) | Delete byte at cursor |
| Ctrl+Z / Cmd+Z | Undo |
| Ctrl+Y / Cmd+Y | Redo |
| **File** ||
| Ctrl+O / Cmd+O | Open file |
| Ctrl+S / Cmd+S | Save file |
| Ctrl+Shift+S / Cmd+Shift+S | Save as |
| **Tabs** ||
| Ctrl+T / Cmd+T | New tab |
| Ctrl+W / Cmd+W | Close tab |
| Ctrl+Tab | Next tab |
| Ctrl+Shift+Tab | Previous tab |
| **Search** ||
| Ctrl+F / Cmd+F | Toggle search |
| Tab (in search) | Switch ASCII/Hex mode |
| Enter / F3 | Next match |
| Shift+F3 | Previous match |
| Backspace | Delete character |
| Esc | Close search / Cancel |
| **Bookmarks** ||
| Ctrl+B / Cmd+B | Toggle bookmark at cursor (with comment input) |
| F2 | Jump to next bookmark |
| Shift+F2 | Jump to previous bookmark |
| Ctrl+Shift+B / Cmd+Shift+B | Edit bookmark comment at cursor |
| **Compare** ||
| Ctrl+K / Cmd+K | Toggle compare mode |
| **Data Inspector** ||
| Ctrl+I / Cmd+I | Toggle data inspector |
| Ctrl+E / Cmd+E | Toggle endianness |
| **Text Encoding** ||
| Ctrl+Shift+E / Cmd+Shift+E | Cycle text encoding |
| **Bitmap** ||
| Ctrl+M / Cmd+M | Toggle bitmap view |
| C (in bitmap) | Cycle color mode |
| + / = | Increase bitmap width |
| - | Decrease bitmap width |

## Installation

### Prerequisites
- Rust 1.70 or later
- macOS, Linux, or Windows

### Build from Source

```bash
# Clone the repository
git clone https://github.com/suma/rusthex.git
cd rusthex

# Build release version
cargo build --release

# The binary will be in target/release/rusthex
```

### Windows Build Script

On Windows, you can use `scripts\build.bat` for building.

```cmd
scripts\build.bat           # Debug build
scripts\build.bat release   # Release build
scripts\build.bat run       # Run the application
scripts\build.bat test      # Run tests
scripts\build.bat check     # Compilation check
scripts\build.bat clean     # Clean build artifacts
scripts\build.bat fmt       # Format code
scripts\build.bat clippy    # Run linter
```

**Note**: Building on Windows requires Visual Studio Build Tools (C++ compiler).

## Usage

```bash
# Open with sample data
cargo run

# Open a specific file
cargo run -- path/to/file.bin

# Or use the compiled binary
./target/release/rusthex path/to/file.bin
```

## Technical Details

### Architecture
- **UI Framework**: gpui 0.2.2 - GPU-accelerated UI framework
- **Memory Management**:
  - Small files (< 10MB): In-memory storage
  - Large files (≥ 10MB): Memory-mapped files (memmap2)
- **Edit Buffer**: Overlay-based modification tracking (HashMap)
- **Search Engine**: memchr crate for SIMD-accelerated pattern matching
- **Performance Optimizations**:
  - Virtual scrolling for large files
  - O(1) search result lookup using HashSet
  - SIMD byte pattern search (AVX2/SSE2 on x86_64, NEON on ARM)

### Project Structure
```
rusthex/
├── src/
│   ├── main.rs         # Main application, HexEditor struct, and Render trait
│   ├── render.rs       # UI render helper methods (header, tabs, hex view, etc.)
│   ├── document.rs     # Document model and file I/O
│   ├── keyboard.rs     # Keyboard event handling
│   ├── search.rs       # Search functionality with wildcard support
│   ├── ui.rs           # UI types and utilities
│   ├── tab.rs          # Editor tab state
│   ├── tabs.rs         # Multi-tab operations
│   ├── cursor.rs       # Cursor movement and navigation
│   ├── bookmark.rs     # Bookmark management
│   ├── file_ops.rs     # File operations and dialogs
│   ├── input.rs        # Hex/ASCII input handling
│   ├── inspector.rs    # Data inspector panel
│   ├── bitmap.rs       # Bitmap visualization
│   ├── config.rs       # Configuration file handling
│   ├── compare.rs      # Compare mode functionality
│   ├── render_cache.rs # Render optimization cache
│   └── encoding.rs     # Text encoding support (ASCII, UTF-8, SJIS, etc.)
├── Cargo.toml
├── LICENSE-MIT
├── LICENSE-APACHE
└── README.md
```

## Performance

RustHex is designed to handle large files efficiently:
- **Virtual Scrolling**: Only renders visible rows, enabling smooth navigation of multi-gigabyte files
- **Memory-Mapped Files**: Large files are not loaded entirely into memory
- **Background Search**: Search operations run in separate threads to keep UI responsive
- **SIMD-Accelerated Search**: Uses memchr crate for vectorized byte pattern matching (AVX2/SSE2/NEON)
  - Exact patterns: Direct SIMD search (10-50x faster than naive)
  - Wildcard patterns: SIMD pre-filtering with prefix matching
- **Optimized Rendering**: HashSet-based lookup for instant search result highlighting
- **Render Cache**: Row-level caching with stable element IDs for efficient UI updates
- **Font Metrics**: Accurate row height calculation using font metrics

## Text Encoding Notes

- **Display Only**: Text encoding setting only affects the ASCII column display. Editing is always byte-based.
- **Layout Preserved**: Each byte occupies one display slot. Multi-byte characters show the decoded character at the first byte position, with `·` markers for continuation bytes.
- **Boundary Handling**: Multi-byte sequences that start mid-row may display as invalid (`.`).
- **No IME Support**: Keyboard input remains ASCII/hex only.

## Development

```bash
# Run in development mode
cargo run

# Run with a specific file
cargo run -- Cargo.toml

# Run tests
cargo test

# Check code quality
cargo clippy

# Format code
cargo fmt
```

## Roadmap

See [todo.md](todo.md) for detailed implementation status and planned features.

### Planned Features
- [ ] Copy/paste support
- [ ] Search & replace
- [ ] Customizable themes
- [ ] Export (C array, Base64, Hex dump)
- [x] Bitmap visualization (implemented)

## License

This project is dual-licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Acknowledgments

Built with [gpui](https://www.gpui.rs/) - A fast, productive UI framework for Rust

---

Copyright (c) 2025 Shuzo Kashihara
