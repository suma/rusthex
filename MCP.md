# rusthex MCP Integration Guide

rusthex-mcp is an MCP server that allows MCP clients such as Claude Code to operate on binary files. When the rusthex GUI is running, it communicates via a Unix domain socket for in-memory editing with undo support. When the GUI is not running, it falls back to direct file I/O.

## Architecture

```
Claude Code ──stdio──▶ rusthex-mcp ──Unix socket──▶ rusthex GUI (in-memory)
                           │
                           └── fallback: direct file I/O
```

- **rusthex-ipc** (`crates/rusthex-ipc/`) — Shared protocol types crate. Both the GUI and MCP server depend on it.
- **rusthex-mcp** (`crates/rusthex-mcp/`) — MCP server (stdio transport)
- **rusthex GUI** (`src/`) — gpui-based hex editor. Automatically starts an IPC server on launch.
- **Socket path** — `$TMPDIR/rusthex.sock` (per-user temp directory on macOS)

## Setup

### 1. Build

```bash
cargo build --release
```

Output binaries:
- GUI: `target/release/rusthex`
- MCP server: `target/release/rusthex-mcp`

### 2. Register the MCP server with Claude Code

Add the following to `.claude/settings.json` or `.mcp.json`:

```json
{
  "mcpServers": {
    "rusthex": {
      "command": "/path/to/target/release/rusthex-mcp"
    }
  }
}
```

### 3. Usage Patterns

#### GUI-connected mode (recommended)

1. Open a file in the rusthex GUI: `./target/release/rusthex /path/to/file.bin`
2. Use MCP tools from Claude Code

When the GUI is running, tools automatically operate on in-memory state. Responses are prefixed with `[via GUI]` to indicate this.

#### Standalone mode

You can also use MCP tools without the GUI for direct file I/O. In this mode, `hex_write` writes directly to disk and `hex_patch` creates a `.bak` backup.

## MCP Tools

### File Operation Tools (with GUI fallback)

These tools automatically use in-memory operations for files open in the GUI. For files not open in the GUI or when the GUI is not running, they fall back to direct file I/O.

| Tool | Parameters | Description |
|---|---|---|
| `hex_read` | `path`, `offset?`, `length?` | Read bytes as hex dump (default: 256 bytes, max: 65536) |
| `hex_write` | `path`, `offset`, `hex_data` | Overwrite bytes at offset. Supports undo when via GUI |
| `hex_search` | `path`, `pattern`, `mode?`, `max_results?` | Search for byte pattern or ASCII string. Supports `??` wildcard |
| `hex_info` | `path` | File metadata (size, magic byte detection, preview). Shows unsaved changes and cursor position when via GUI |
| `hex_interpret` | `path`, `offset` | Interpret bytes at offset as u8/u16/u32/u64/i8-i64/f32/f64/ASCII/UTF-8 |
| `hex_patch` | `path`, `offset`, `delete_count`, `insert_data?` | Delete and/or insert bytes. Supports undo when via GUI; creates `.bak` backup for direct I/O |

### GUI-Only Tools

These tools require the rusthex GUI to be running. They return an error message if the GUI is not available.

| Tool | Parameters | Description |
|---|---|---|
| `gui_list_tabs` | (none) | List all open tabs (path, size, unsaved changes, active status) |
| `gui_goto` | `path`, `offset` | Switch to the tab containing the file and move the cursor to the specified offset |
| `gui_undo` | `path` | Undo the last edit on the specified file |
| `gui_redo` | `path` | Redo the last undone edit on the specified file |

## Usage Examples

### Read bytes from a file

```
hex_read(path: "/path/to/binary", offset: 0x100, length: 64)
```

### Write and undo via GUI

```
hex_write(path: "/path/to/binary", offset: 0x00, hex_data: "90 90 90")
gui_undo(path: "/path/to/binary")
```

### Search for a pattern

```
hex_search(path: "/path/to/binary", pattern: "4d 5a ?? 00", mode: "hex")
hex_search(path: "/path/to/binary", pattern: "Hello", mode: "ascii")
```

### Patch: delete 2 bytes and insert 4 bytes

```
hex_patch(path: "/path/to/binary", offset: 0x10, delete_count: 2, insert_data: "DE AD BE EF")
```

### Navigate in GUI

```
gui_list_tabs()
gui_goto(path: "/path/to/binary", offset: 0xFF00)
```

## IPC Protocol Details

### Socket

- Path: `std::env::temp_dir().join("rusthex.sock")` (macOS: `/var/folders/.../T/rusthex.sock`)
- Protocol: JSON Lines (1 connection = 1 request + 1 response, then disconnect)

### Manual Testing

You can verify IPC connectivity while the GUI is running with the following commands:

```bash
# Ping
echo '{"method":"Ping"}' | nc -U $TMPDIR/rusthex.sock

# List tabs
echo '{"method":"ListTabs"}' | nc -U $TMPDIR/rusthex.sock

# Read bytes
echo '{"method":"ReadBytes","params":{"path":"/path/to/file","offset":0,"length":16}}' | nc -U $TMPDIR/rusthex.sock
```

### Request Format

```json
{"method":"<MethodName>","params":{...}}
```

Params vary by method. `Ping` and `ListTabs` have no params.

### Response Format

```json
{"ok":true,"result":{"type":"<ResultType>",...}}
{"ok":false,"error":"error message"}
```

## Limitations

- **Unix only** — v1 supports Unix domain sockets only. Windows named pipes are not yet supported.
- **Single instance** — The socket path is fixed, so only the most recently launched GUI instance responds.
- **Polling** — gpui lacks an external FD waiting API, so the IPC receiver polls at 50ms intervals (negligible CPU overhead).
- **Search** — Uses `doc.to_vec()` to materialize the full document, which may take several seconds for large files (10-second timeout).
