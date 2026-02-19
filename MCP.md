# rusthex MCP Integration Guide

rusthex-mcp is an MCP server that allows MCP clients such as Claude Code to operate on binary files. When the rusthex GUI is running, it communicates via a Unix domain socket for in-memory editing with undo support. When the GUI is not running, it falls back to direct file I/O.

## Architecture

```
Claude Code ──stdio──▶ rusthex-mcp ──Unix socket──▶ rusthex GUI (in-memory)
                           │
                           └── fallback: direct file I/O
```

The system consists of three crates:

| Crate | Path | Role |
|---|---|---|
| **rusthex** (GUI) | `src/` | gpui-based hex editor. Starts an IPC server on launch. |
| **rusthex-mcp** | `crates/rusthex-mcp/` | MCP server binary (stdio transport). Connects to GUI via socket. |
| **rusthex-ipc** | `crates/rusthex-ipc/` | Shared library defining protocol types and `send_request()`. |

## Building

```bash
cargo build --release
```

Output binaries:
- GUI: `target/release/rusthex`
- MCP server: `target/release/rusthex-mcp`

## rusthex GUI

### Running

```bash
# Open with a file
./target/release/rusthex /path/to/file.bin

# Open with sample data
./target/release/rusthex
```

### IPC Server

The GUI automatically starts a Unix domain socket listener at `$TMPDIR/rusthex.sock` on launch. No configuration is needed. The socket file is cleaned up when the GUI quits normally.

If a stale socket file exists (e.g. from a crash), the GUI removes it on startup.

### Keyboard Shortcuts

| Shortcut | Action |
|---|---|
| `Cmd-O` | Open file |
| `Cmd-S` | Save |
| `Cmd-Shift-S` | Save as |
| `Cmd-T` | New tab |
| `Cmd-W` | Close tab |
| `Cmd-Z` | Undo |
| `Cmd-Y` | Redo |
| `Cmd-C` | Copy |
| `Cmd-V` | Paste |
| `Cmd-A` | Select all |
| `Cmd-F` | Toggle search |
| `Cmd-I` | Toggle data inspector |
| `Cmd-M` | Toggle bitmap view |
| `Cmd-P` | Toggle pattern panel |
| `Cmd-K` | Toggle compare mode |
| `Cmd-Shift-I` | Toggle insert mode |
| `Cmd-Shift-E` | Cycle text encoding |

## rusthex-mcp

### Registering with Claude Code

Add to `.claude/settings.json` or `.mcp.json`:

```json
{
  "mcpServers": {
    "rusthex": {
      "command": "/path/to/target/release/rusthex-mcp"
    }
  }
}
```

### Usage Patterns

#### GUI-connected mode (recommended)

1. Open a file in the rusthex GUI: `./target/release/rusthex /path/to/file.bin`
2. Use MCP tools from Claude Code

When the GUI is running, tools automatically operate on in-memory state. Responses are prefixed with `[via GUI]`.

#### Standalone mode

MCP tools work without the GUI via direct file I/O. In this mode, `hex_write` writes directly to disk and `hex_patch` creates a `.bak` backup. No undo support is available.

### MCP Tools

#### File Operation Tools (with GUI fallback)

These tools automatically use in-memory operations for files open in the GUI. For files not open in the GUI or when the GUI is not running, they fall back to direct file I/O.

| Tool | Parameters | Description |
|---|---|---|
| `hex_read` | `path`, `offset?`, `length?` | Read bytes as hex dump (default: 256 bytes, max: 65536) |
| `hex_write` | `path`, `offset`, `hex_data` | Overwrite bytes at offset. Supports undo when via GUI |
| `hex_search` | `path`, `pattern`, `mode?`, `max_results?` | Search for byte pattern or ASCII string. Supports `??` wildcard |
| `hex_info` | `path` | File metadata (size, magic byte detection, preview). Shows unsaved changes and cursor position when via GUI |
| `hex_interpret` | `path`, `offset` | Interpret bytes at offset as u8/u16/u32/u64/i8-i64/f32/f64/ASCII/UTF-8 |
| `hex_patch` | `path`, `offset`, `delete_count`, `insert_data?` | Delete and/or insert bytes. Supports undo when via GUI; creates `.bak` backup for direct I/O |

#### GUI-Only Tools

These tools require the rusthex GUI to be running. They return an error message if the GUI is not available.

| Tool | Parameters | Description |
|---|---|---|
| `gui_list_tabs` | (none) | List all open tabs (path, size, unsaved changes, active status) |
| `gui_goto` | `path`, `offset` | Switch to the tab containing the file and move the cursor to the specified offset |
| `gui_undo` | `path` | Undo the last edit on the specified file |
| `gui_redo` | `path` | Redo the last undone edit on the specified file |

### Tool Examples

```
# Read 64 bytes at offset 0x100
hex_read(path: "/path/to/binary", offset: 0x100, length: 64)

# Overwrite bytes, then undo
hex_write(path: "/path/to/binary", offset: 0x00, hex_data: "90 90 90")
gui_undo(path: "/path/to/binary")

# Search for hex pattern with wildcard
hex_search(path: "/path/to/binary", pattern: "4d 5a ?? 00", mode: "hex")

# Search for ASCII string
hex_search(path: "/path/to/binary", pattern: "Hello", mode: "ascii")

# Delete 2 bytes and insert 4 bytes at offset 0x10
hex_patch(path: "/path/to/binary", offset: 0x10, delete_count: 2, insert_data: "DE AD BE EF")

# List open tabs and navigate
gui_list_tabs()
gui_goto(path: "/path/to/binary", offset: 0xFF00)
```

## rusthex-ipc

### Overview

`rusthex-ipc` is a library crate that defines the IPC protocol types shared by the GUI and MCP server. It can also be used by third-party tools to communicate with the rusthex GUI.

### Dependency

```toml
[dependencies]
rusthex-ipc = { path = "crates/rusthex-ipc" }
```

### API

```rust
use rusthex_ipc::{socket_path, send_request, IpcRequest, IpcResponse, IpcResult};

// Get the socket path
let path = socket_path();  // e.g. /var/folders/.../T/rusthex.sock

// Send a request to the GUI (returns None if GUI is not running)
if let Some(resp) = send_request(&IpcRequest::Ping) {
    assert!(resp.ok);
    // resp.result == Some(IpcResult::Pong)
}

// Read bytes from a file open in the GUI
let resp = send_request(&IpcRequest::ReadBytes {
    path: "/path/to/file.bin".into(),
    offset: 0,
    length: 256,
});
match resp {
    Some(r) if r.ok => { /* use r.result */ }
    Some(r) => eprintln!("error: {}", r.error.unwrap_or_default()),
    None => eprintln!("GUI is not running"),
}
```

### IPC Protocol

- **Socket path**: `std::env::temp_dir().join("rusthex.sock")`
- **Transport**: Unix domain socket
- **Framing**: JSON Lines (one JSON object per line)
- **Connection model**: 1 connection = 1 request + 1 response, then disconnect

#### Request Format

```json
{"method":"<MethodName>","params":{...}}
```

`Ping` and `ListTabs` have no params field.

#### Response Format

```json
{"ok":true,"result":{"type":"<ResultType>",...}}
{"ok":false,"error":"error message"}
```

#### IPC Methods

| Method | Params | Result | Description |
|---|---|---|---|
| `Ping` | — | `Pong` | Health check |
| `ListTabs` | — | `Tabs { tabs }` | List all open tabs |
| `ReadBytes` | `path`, `offset`, `length` | `Bytes { data, raw_hex, length }` | Read bytes as hex dump |
| `WriteBytes` | `path`, `offset`, `data` (hex) | `WriteOk { bytes_written }` | Overwrite bytes |
| `InsertBytes` | `path`, `offset`, `data` (hex) | `InsertOk { bytes_inserted }` | Insert bytes at offset |
| `DeleteBytes` | `path`, `offset`, `count` | `DeleteOk { bytes_deleted }` | Delete bytes at offset |
| `SearchBytes` | `path`, `pattern`, `mode?`, `max_results?` | `SearchResults { matches, total }` | Search for pattern |
| `FileInfo` | `path` | `FileInfo { path, size, has_unsaved_changes, cursor_position }` | File metadata |
| `InterpretBytes` | `path`, `offset` | `Interpretation { text }` | Multi-type interpretation |
| `Goto` | `path`, `offset` | `GotoOk { offset }` | Move cursor |
| `Undo` | `path` | `UndoOk { offset? }` | Undo last edit |
| `Redo` | `path` | `RedoOk { offset? }` | Redo last undo |

#### Manual Testing with nc

```bash
# Ping
echo '{"method":"Ping"}' | nc -U $TMPDIR/rusthex.sock

# List tabs
echo '{"method":"ListTabs"}' | nc -U $TMPDIR/rusthex.sock

# Read 16 bytes at offset 0
echo '{"method":"ReadBytes","params":{"path":"/path/to/file","offset":0,"length":16}}' | nc -U $TMPDIR/rusthex.sock

# Write bytes
echo '{"method":"WriteBytes","params":{"path":"/path/to/file","offset":0,"data":"deadbeef"}}' | nc -U $TMPDIR/rusthex.sock

# Undo
echo '{"method":"Undo","params":{"path":"/path/to/file"}}' | nc -U $TMPDIR/rusthex.sock

# Navigate cursor
echo '{"method":"Goto","params":{"path":"/path/to/file","offset":256}}' | nc -U $TMPDIR/rusthex.sock
```

### Testing

```bash
# Protocol type serialization tests
cargo test -p rusthex-ipc

# MCP server tests (direct file I/O path)
cargo test -p rusthex-mcp
```

## Limitations

- **Unix only** — v1 supports Unix domain sockets only. Windows named pipes are not yet supported.
- **Single instance** — The socket path is fixed, so only the most recently launched GUI instance responds.
- **Polling** — gpui lacks an external FD waiting API, so the IPC receiver polls at 50ms intervals (negligible CPU overhead).
- **Search** — Uses `doc.to_vec()` to materialize the full document, which may take several seconds for large files (10-second timeout).
