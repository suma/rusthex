use std::fs;
use std::io::{Read, Seek, SeekFrom, Write};
use memchr::memmem;
use memmap2::Mmap;
use rmcp::{
    ServerHandler,
    handler::server::{router::tool::ToolRouter, wrapper::Parameters},
    model::{CallToolResult, Content, Implementation, ProtocolVersion, ServerCapabilities, ServerInfo},
    schemars, tool, tool_handler, tool_router,
};
use rusthex_ipc::{IpcRequest, IpcResult, send_request};
use serde::Deserialize;

type McpError = rmcp::ErrorData;

// ---------------------------------------------------------------------------
// Parameter structs
// ---------------------------------------------------------------------------

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct HexReadParams {
    #[schemars(description = "Path to the binary file")]
    pub path: String,
    #[schemars(description = "Byte offset to start reading from (default: 0)")]
    pub offset: Option<u64>,
    #[schemars(description = "Number of bytes to read (default: 256, max: 65536)")]
    pub length: Option<usize>,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct HexWriteParams {
    #[schemars(description = "Path to the binary file")]
    pub path: String,
    #[schemars(description = "Byte offset to write at")]
    pub offset: u64,
    #[schemars(description = "Hex string of bytes to write (e.g. \"48 65 6c 6c 6f\" or \"48656c6c6f\")")]
    pub hex_data: String,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct HexSearchParams {
    #[schemars(description = "Path to the binary file")]
    pub path: String,
    #[schemars(description = "Pattern to search for. Hex mode: space-separated hex bytes (e.g. \"4d 5a 90 00\"), use \"??\" for wildcard. ASCII mode: literal string.")]
    pub pattern: String,
    #[schemars(description = "Search mode: \"hex\" or \"ascii\" (default: \"hex\")")]
    pub mode: Option<String>,
    #[schemars(description = "Maximum number of results to return (default: 20)")]
    pub max_results: Option<usize>,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct HexInfoParams {
    #[schemars(description = "Path to the binary file")]
    pub path: String,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct HexInterpretParams {
    #[schemars(description = "Path to the binary file")]
    pub path: String,
    #[schemars(description = "Byte offset to interpret from")]
    pub offset: u64,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct HexPatchParams {
    #[schemars(description = "Path to the binary file")]
    pub path: String,
    #[schemars(description = "Byte offset to start the patch")]
    pub offset: u64,
    #[schemars(description = "Number of bytes to delete at offset")]
    pub delete_count: usize,
    #[schemars(description = "Hex string of bytes to insert at offset (optional, e.g. \"48 65 6c 6c 6f\")")]
    pub insert_data: Option<String>,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct FileTypeParams {
    #[schemars(description = "Path to the binary file")]
    pub path: String,
}

// GUI-only tool parameters

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct GuiListTabsParams {}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct GuiGotoParams {
    #[schemars(description = "Path to the file (must be open in GUI)")]
    pub path: String,
    #[schemars(description = "Byte offset to navigate to")]
    pub offset: u64,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct GuiUndoParams {
    #[schemars(description = "Path to the file (must be open in GUI)")]
    pub path: String,
}

#[derive(Debug, Deserialize, schemars::JsonSchema)]
pub struct GuiRedoParams {
    #[schemars(description = "Path to the file (must be open in GUI)")]
    pub path: String,
}

// ---------------------------------------------------------------------------
// HexServer
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct HexServer {
    tool_router: ToolRouter<HexServer>,
}

#[tool_router]
impl HexServer {
    pub fn new() -> Self {
        Self {
            tool_router: Self::tool_router(),
        }
    }

    #[tool(description = "Read bytes from a binary file as hex dump. If the file is open in rusthex GUI, reads in-memory state including unsaved edits.")]
    async fn hex_read(
        &self,
        Parameters(p): Parameters<HexReadParams>,
    ) -> Result<CallToolResult, McpError> {
        let offset = p.offset.unwrap_or(0);
        let length = p.length.unwrap_or(256).min(65536);

        // Try GUI first
        if let Some(resp) = send_request(&IpcRequest::ReadBytes {
            path: p.path.clone(),
            offset,
            length,
        }) {
            if resp.ok {
                if let Some(IpcResult::Bytes { data, .. }) = resp.result {
                    return Ok(CallToolResult::success(vec![Content::text(
                        format!("[via GUI] {}", data),
                    )]));
                }
            }
            // "file not open" error -> fall through to direct I/O
        }

        let data = read_bytes_from_file(&p.path, offset, length)
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        if data.is_empty() {
            return Ok(CallToolResult::success(vec![Content::text(
                "No data at specified offset (beyond end of file).",
            )]));
        }

        let dump = format_hex_dump(&data, offset);
        Ok(CallToolResult::success(vec![Content::text(dump)]))
    }

    #[tool(description = "Overwrite bytes at offset in a binary file. If the file is open in rusthex GUI, writes to in-memory buffer with undo support.")]
    async fn hex_write(
        &self,
        Parameters(p): Parameters<HexWriteParams>,
    ) -> Result<CallToolResult, McpError> {
        let bytes = parse_hex_string(&p.hex_data)
            .map_err(|e| McpError::invalid_params(e, None))?;

        if bytes.is_empty() {
            return Err(McpError::invalid_params("hex_data is empty", None));
        }

        // Try GUI first
        if let Some(resp) = send_request(&IpcRequest::WriteBytes {
            path: p.path.clone(),
            offset: p.offset,
            data: bytes.clone(),
        }) {
            if resp.ok {
                if let Some(IpcResult::WriteOk { bytes_written }) = resp.result {
                    return Ok(CallToolResult::success(vec![Content::text(format!(
                        "[via GUI] Wrote {} byte(s) at offset 0x{:X}. (undo available)",
                        bytes_written, p.offset
                    ))]));
                }
            }
        }

        write_bytes_to_file(&p.path, p.offset, &bytes)
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        Ok(CallToolResult::success(vec![Content::text(format!(
            "Wrote {} byte(s) at offset 0x{:X}.",
            bytes.len(),
            p.offset
        ))]))
    }

    #[tool(description = "Search for byte pattern or ASCII string in a binary file. If the file is open in rusthex GUI, searches in-memory state.")]
    async fn hex_search(
        &self,
        Parameters(p): Parameters<HexSearchParams>,
    ) -> Result<CallToolResult, McpError> {
        let mode = p.mode.as_deref().unwrap_or("hex");
        let max_results = p.max_results.unwrap_or(20).min(1000);

        // Try GUI first
        if let Some(resp) = send_request(&IpcRequest::SearchBytes {
            path: p.path.clone(),
            pattern: p.pattern.clone(),
            mode: Some(mode.to_string()),
            max_results: Some(max_results),
        }) {
            if resp.ok {
                if let Some(IpcResult::SearchResults { matches, total }) = resp.result {
                    if matches.is_empty() {
                        return Ok(CallToolResult::success(vec![Content::text(
                            "[via GUI] No matches found.",
                        )]));
                    }
                    let mut output = format!("[via GUI] Found {} match(es):\n", total);
                    for offset in &matches {
                        output.push_str(&format!("  0x{:08X}\n", offset));
                    }
                    return Ok(CallToolResult::success(vec![Content::text(output)]));
                }
            }
        }

        let file_data = read_entire_file(&p.path)
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        let results = match mode {
            "ascii" => {
                let pattern = p.pattern.as_bytes();
                search_bytes(&file_data, pattern, max_results)
            }
            _ => {
                let parsed = parse_hex_pattern(&p.pattern)
                    .map_err(|e| McpError::invalid_params(e, None))?;
                if parsed.iter().any(|b| b.is_none()) {
                    // Wildcard search
                    search_bytes_wildcard(&file_data, &parsed, max_results)
                } else {
                    let concrete: Vec<u8> = parsed.into_iter().map(|b| b.unwrap()).collect();
                    search_bytes(&file_data, &concrete, max_results)
                }
            }
        };

        if results.is_empty() {
            return Ok(CallToolResult::success(vec![Content::text(
                "No matches found.",
            )]));
        }

        let mut output = format!("Found {} match(es):\n", results.len());
        for offset in &results {
            let end = (*offset + 16).min(file_data.len());
            let context = &file_data[*offset..end];
            let hex: Vec<String> = context.iter().map(|b| format!("{:02x}", b)).collect();
            output.push_str(&format!("  0x{:08X}: {}\n", offset, hex.join(" ")));
        }
        Ok(CallToolResult::success(vec![Content::text(output)]))
    }

    #[tool(description = "Get binary file metadata and preview. If the file is open in rusthex GUI, shows in-memory state including unsaved changes status.")]
    async fn hex_info(
        &self,
        Parameters(p): Parameters<HexInfoParams>,
    ) -> Result<CallToolResult, McpError> {
        // Try GUI first
        if let Some(resp) = send_request(&IpcRequest::FileInfo {
            path: p.path.clone(),
        }) {
            if resp.ok {
                if let Some(IpcResult::FileInfo {
                    path,
                    size,
                    has_unsaved_changes,
                    cursor_position,
                }) = resp.result
                {
                    let mut output = format!("[via GUI] File: {}\n", path);
                    output.push_str(&format!("Size: {} bytes", size));
                    if size >= 1024 * 1024 {
                        output.push_str(&format!(" ({:.2} MB)", size as f64 / (1024.0 * 1024.0)));
                    } else if size >= 1024 {
                        output.push_str(&format!(" ({:.1} KB)", size as f64 / 1024.0));
                    }
                    output.push('\n');
                    output.push_str(&format!("Unsaved changes: {}\n", has_unsaved_changes));
                    output.push_str(&format!("Cursor position: 0x{:X}\n", cursor_position));
                    return Ok(CallToolResult::success(vec![Content::text(output)]));
                }
            }
        }

        let metadata = fs::metadata(&p.path)
            .map_err(|e| McpError::internal_error(format!("Cannot stat file: {}", e), None))?;

        let size = metadata.len();
        let preview_len = 64.min(size as usize);
        let preview = read_bytes_from_file(&p.path, 0, preview_len)
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        // Detect magic bytes
        let magic = detect_magic(&preview);

        let mut output = String::new();
        output.push_str(&format!("File: {}\n", p.path));
        output.push_str(&format!("Size: {} bytes", size));
        if size >= 1024 * 1024 {
            output.push_str(&format!(" ({:.2} MB)", size as f64 / (1024.0 * 1024.0)));
        } else if size >= 1024 {
            output.push_str(&format!(" ({:.1} KB)", size as f64 / 1024.0));
        }
        output.push('\n');

        if let Some(magic) = magic {
            output.push_str(&format!("Type: {}\n", magic));
        }

        output.push_str(&format!("\nFirst {} bytes:\n", preview_len));
        output.push_str(&format_hex_dump(&preview, 0));

        Ok(CallToolResult::success(vec![Content::text(output)]))
    }

    #[tool(description = "Interpret bytes at offset as multiple typed values (u8, u16, u32, u64, i8, i16, i32, i64, f32, f64, ASCII, UTF-8). If the file is open in rusthex GUI, reads in-memory state.")]
    async fn hex_interpret(
        &self,
        Parameters(p): Parameters<HexInterpretParams>,
    ) -> Result<CallToolResult, McpError> {
        // Try GUI first
        if let Some(resp) = send_request(&IpcRequest::InterpretBytes {
            path: p.path.clone(),
            offset: p.offset,
        }) {
            if resp.ok {
                if let Some(IpcResult::Interpretation { text }) = resp.result {
                    return Ok(CallToolResult::success(vec![Content::text(
                        format!("[via GUI] {}", text),
                    )]));
                }
            }
        }

        let data = read_bytes_from_file(&p.path, p.offset, 8)
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        if data.is_empty() {
            return Err(McpError::invalid_params("Offset beyond end of file", None));
        }

        let output = interpret_bytes(&data, p.offset);
        Ok(CallToolResult::success(vec![Content::text(output)]))
    }

    #[tool(description = "Patch file: delete and/or insert bytes at offset. If the file is open in rusthex GUI, applies edits with undo support. Otherwise creates .bak backup.")]
    async fn hex_patch(
        &self,
        Parameters(p): Parameters<HexPatchParams>,
    ) -> Result<CallToolResult, McpError> {
        let insert_bytes = match &p.insert_data {
            Some(hex) => parse_hex_string(hex)
                .map_err(|e| McpError::invalid_params(e, None))?,
            None => vec![],
        };

        if p.delete_count == 0 && insert_bytes.is_empty() {
            return Err(McpError::invalid_params(
                "Nothing to do: delete_count is 0 and no insert_data",
                None,
            ));
        }

        // Try GUI: delete then insert
        let gui_available = send_request(&IpcRequest::FileInfo {
            path: p.path.clone(),
        })
        .is_some_and(|r| r.ok);

        if gui_available {
            let mut msg = format!("[via GUI] Patched {}\n", p.path);

            if p.delete_count > 0 {
                match send_request(&IpcRequest::DeleteBytes {
                    path: p.path.clone(),
                    offset: p.offset,
                    count: p.delete_count,
                }) {
                    Some(resp) if resp.ok => {
                        msg.push_str(&format!(
                            "  Deleted {} byte(s) at offset 0x{:X}\n",
                            p.delete_count, p.offset
                        ));
                    }
                    Some(resp) => {
                        return Ok(CallToolResult::success(vec![Content::text(format!(
                            "[via GUI] Delete failed: {}",
                            resp.error.unwrap_or_default()
                        ))]));
                    }
                    None => {} // GUI disconnected, fall through
                }
            }

            if !insert_bytes.is_empty() {
                match send_request(&IpcRequest::InsertBytes {
                    path: p.path.clone(),
                    offset: p.offset,
                    data: insert_bytes.clone(),
                }) {
                    Some(resp) if resp.ok => {
                        msg.push_str(&format!(
                            "  Inserted {} byte(s) at offset 0x{:X}\n",
                            insert_bytes.len(),
                            p.offset
                        ));
                    }
                    Some(resp) => {
                        return Ok(CallToolResult::success(vec![Content::text(format!(
                            "[via GUI] Insert failed: {}",
                            resp.error.unwrap_or_default()
                        ))]));
                    }
                    None => {}
                }
            }

            msg.push_str("  (undo available)\n");
            return Ok(CallToolResult::success(vec![Content::text(msg)]));
        }

        patch_file(&p.path, p.offset, p.delete_count, &insert_bytes)
            .map_err(|e| McpError::internal_error(e.to_string(), None))?;

        let mut msg = format!("Patched {} (backup: {}.bak)\n", p.path, p.path);
        if p.delete_count > 0 {
            msg.push_str(&format!(
                "  Deleted {} byte(s) at offset 0x{:X}\n",
                p.delete_count, p.offset
            ));
        }
        if !insert_bytes.is_empty() {
            msg.push_str(&format!(
                "  Inserted {} byte(s) at offset 0x{:X}\n",
                insert_bytes.len(),
                p.offset
            ));
        }

        Ok(CallToolResult::success(vec![Content::text(msg)]))
    }

    #[tool(description = "Detect file type using the `file` command. Reads the first 1024 bytes and pipes them to `file -b -` for identification.")]
    async fn detect_file_type(
        &self,
        Parameters(p): Parameters<FileTypeParams>,
    ) -> Result<CallToolResult, McpError> {
        let data = read_bytes_from_file(&p.path, 0, 1024)
            .map_err(|e| McpError::internal_error(format!("Cannot read file: {}", e), None))?;

        if data.is_empty() {
            return Ok(CallToolResult::success(vec![Content::text(
                "Empty file.",
            )]));
        }

        let result = std::process::Command::new("file")
            .args(["-b", "-"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .and_then(|mut child| {
                if let Some(ref mut stdin) = child.stdin {
                    let _ = stdin.write_all(&data);
                }
                drop(child.stdin.take());
                child.wait_with_output()
            });

        match result {
            Ok(output) if output.status.success() => {
                let file_type = String::from_utf8_lossy(&output.stdout).trim().to_string();
                Ok(CallToolResult::success(vec![Content::text(format!(
                    "{}: {}",
                    p.path, file_type
                ))]))
            }
            Ok(output) => {
                let stderr = String::from_utf8_lossy(&output.stderr).trim().to_string();
                Err(McpError::internal_error(
                    format!("file command failed: {}", stderr),
                    None,
                ))
            }
            Err(e) => Err(McpError::internal_error(
                format!("file command not available: {}", e),
                None,
            )),
        }
    }

    // -----------------------------------------------------------------------
    // GUI-only tools
    // -----------------------------------------------------------------------

    #[tool(description = "List all tabs open in the rusthex GUI editor. Returns tab index, file path, name, size, and unsaved changes status. Requires rusthex GUI to be running.")]
    async fn gui_list_tabs(
        &self,
        #[allow(unused_variables)]
        Parameters(_p): Parameters<GuiListTabsParams>,
    ) -> Result<CallToolResult, McpError> {
        match send_request(&IpcRequest::ListTabs) {
            Some(resp) if resp.ok => {
                if let Some(IpcResult::Tabs { tabs }) = resp.result {
                    if tabs.is_empty() {
                        return Ok(CallToolResult::success(vec![Content::text(
                            "[via GUI] No tabs open.",
                        )]));
                    }
                    let mut output = format!("[via GUI] {} tab(s) open:\n", tabs.len());
                    for tab in &tabs {
                        let active = if tab.is_active { " (active)" } else { "" };
                        let modified = if tab.has_unsaved_changes { " [modified]" } else { "" };
                        let path = tab
                            .path
                            .as_deref()
                            .unwrap_or("(no file)");
                        output.push_str(&format!(
                            "  [{}] {}{}{} ({} bytes)\n      path: {}\n",
                            tab.index, tab.name, active, modified, tab.size, path,
                        ));
                    }
                    Ok(CallToolResult::success(vec![Content::text(output)]))
                } else {
                    Ok(CallToolResult::success(vec![Content::text(
                        "[via GUI] Unexpected response format.",
                    )]))
                }
            }
            Some(resp) => Ok(CallToolResult::success(vec![Content::text(format!(
                "GUI error: {}",
                resp.error.unwrap_or_default()
            ))])),
            None => Ok(CallToolResult::success(vec![Content::text(
                "rusthex GUI is not running. Start the GUI first to use this tool.",
            )])),
        }
    }

    #[tool(description = "Move the cursor to a specific byte offset in the rusthex GUI editor. Also switches to the tab containing the file. Requires rusthex GUI to be running.")]
    async fn gui_goto(
        &self,
        Parameters(p): Parameters<GuiGotoParams>,
    ) -> Result<CallToolResult, McpError> {
        match send_request(&IpcRequest::Goto {
            path: p.path.clone(),
            offset: p.offset,
        }) {
            Some(resp) if resp.ok => {
                if let Some(IpcResult::GotoOk { offset }) = resp.result {
                    Ok(CallToolResult::success(vec![Content::text(format!(
                        "[via GUI] Cursor moved to offset 0x{:X} in {}",
                        offset, p.path
                    ))]))
                } else {
                    Ok(CallToolResult::success(vec![Content::text(
                        "[via GUI] Cursor moved.",
                    )]))
                }
            }
            Some(resp) => Ok(CallToolResult::success(vec![Content::text(format!(
                "GUI error: {}",
                resp.error.unwrap_or_default()
            ))])),
            None => Ok(CallToolResult::success(vec![Content::text(
                "rusthex GUI is not running. Start the GUI first to use this tool.",
            )])),
        }
    }

    #[tool(description = "Undo the last edit operation in the rusthex GUI editor. Requires rusthex GUI to be running.")]
    async fn gui_undo(
        &self,
        Parameters(p): Parameters<GuiUndoParams>,
    ) -> Result<CallToolResult, McpError> {
        match send_request(&IpcRequest::Undo {
            path: p.path.clone(),
        }) {
            Some(resp) if resp.ok => {
                if let Some(IpcResult::UndoOk { offset }) = resp.result {
                    match offset {
                        Some(off) => Ok(CallToolResult::success(vec![Content::text(format!(
                            "[via GUI] Undo successful. Cursor at offset 0x{:X}",
                            off
                        ))])),
                        None => Ok(CallToolResult::success(vec![Content::text(
                            "[via GUI] Nothing to undo.",
                        )])),
                    }
                } else {
                    Ok(CallToolResult::success(vec![Content::text(
                        "[via GUI] Undo completed.",
                    )]))
                }
            }
            Some(resp) => Ok(CallToolResult::success(vec![Content::text(format!(
                "GUI error: {}",
                resp.error.unwrap_or_default()
            ))])),
            None => Ok(CallToolResult::success(vec![Content::text(
                "rusthex GUI is not running. Start the GUI first to use this tool.",
            )])),
        }
    }

    #[tool(description = "Redo the last undone edit operation in the rusthex GUI editor. Requires rusthex GUI to be running.")]
    async fn gui_redo(
        &self,
        Parameters(p): Parameters<GuiRedoParams>,
    ) -> Result<CallToolResult, McpError> {
        match send_request(&IpcRequest::Redo {
            path: p.path.clone(),
        }) {
            Some(resp) if resp.ok => {
                if let Some(IpcResult::RedoOk { offset }) = resp.result {
                    match offset {
                        Some(off) => Ok(CallToolResult::success(vec![Content::text(format!(
                            "[via GUI] Redo successful. Cursor at offset 0x{:X}",
                            off
                        ))])),
                        None => Ok(CallToolResult::success(vec![Content::text(
                            "[via GUI] Nothing to redo.",
                        )])),
                    }
                } else {
                    Ok(CallToolResult::success(vec![Content::text(
                        "[via GUI] Redo completed.",
                    )]))
                }
            }
            Some(resp) => Ok(CallToolResult::success(vec![Content::text(format!(
                "GUI error: {}",
                resp.error.unwrap_or_default()
            ))])),
            None => Ok(CallToolResult::success(vec![Content::text(
                "rusthex GUI is not running. Start the GUI first to use this tool.",
            )])),
        }
    }
}

#[tool_handler]
impl ServerHandler for HexServer {
    fn get_info(&self) -> ServerInfo {
        ServerInfo {
            protocol_version: ProtocolVersion::V_2024_11_05,
            capabilities: ServerCapabilities::builder().enable_tools().build(),
            server_info: Implementation {
                name: "rusthex-mcp".into(),
                version: env!("CARGO_PKG_VERSION").into(),
                ..Default::default()
            },
            instructions: Some("Binary file analysis and editing tools for hex inspection, searching, interpreting, and patching. When rusthex GUI is running, tools operate on in-memory state with undo support.".to_string()),
        }
    }
}

// ---------------------------------------------------------------------------
// Helper functions
// ---------------------------------------------------------------------------

const MMAP_THRESHOLD: u64 = 10 * 1024 * 1024; // 10 MB

fn read_bytes_from_file(path: &str, offset: u64, length: usize) -> std::io::Result<Vec<u8>> {
    let metadata = fs::metadata(path)?;
    let file_size = metadata.len();

    if offset >= file_size {
        return Ok(vec![]);
    }

    let actual_len = length.min((file_size - offset) as usize);

    if file_size >= MMAP_THRESHOLD {
        let file = fs::File::open(path)?;
        let mmap = unsafe { Mmap::map(&file)? };
        let start = offset as usize;
        let end = start + actual_len;
        Ok(mmap[start..end].to_vec())
    } else {
        let mut file = fs::File::open(path)?;
        file.seek(SeekFrom::Start(offset))?;
        let mut buf = vec![0u8; actual_len];
        file.read_exact(&mut buf)?;
        Ok(buf)
    }
}

fn read_entire_file(path: &str) -> std::io::Result<Vec<u8>> {
    let metadata = fs::metadata(path)?;
    let file_size = metadata.len();

    if file_size >= MMAP_THRESHOLD {
        let file = fs::File::open(path)?;
        let mmap = unsafe { Mmap::map(&file)? };
        Ok(mmap[..].to_vec())
    } else {
        fs::read(path)
    }
}

fn format_hex_dump(data: &[u8], base_offset: u64) -> String {
    let mut output = String::new();
    for (i, chunk) in data.chunks(16).enumerate() {
        let addr = base_offset + (i * 16) as u64;
        output.push_str(&format!("{:08X}  ", addr));

        // Hex part
        for (j, byte) in chunk.iter().enumerate() {
            if j == 8 {
                output.push(' ');
            }
            output.push_str(&format!("{:02x} ", byte));
        }

        // Padding for incomplete lines
        if chunk.len() < 16 {
            let missing = 16 - chunk.len();
            for j in 0..missing {
                if chunk.len() + j == 8 {
                    output.push(' ');
                }
                output.push_str("   ");
            }
        }

        // ASCII part
        output.push(' ');
        for byte in chunk {
            if byte.is_ascii_graphic() || *byte == b' ' {
                output.push(*byte as char);
            } else {
                output.push('.');
            }
        }
        output.push('\n');
    }
    output
}

fn parse_hex_string(s: &str) -> Result<Vec<u8>, String> {
    let cleaned: String = s.chars().filter(|c| !c.is_whitespace()).collect();

    if cleaned.is_empty() {
        return Err("Empty hex string".to_string());
    }

    if !cleaned.len().is_multiple_of(2) {
        return Err("Hex string must have even number of characters".to_string());
    }

    let mut bytes = Vec::with_capacity(cleaned.len() / 2);
    for i in (0..cleaned.len()).step_by(2) {
        let byte_str = &cleaned[i..i + 2];
        let byte = u8::from_str_radix(byte_str, 16)
            .map_err(|_| format!("Invalid hex byte: '{}'", byte_str))?;
        bytes.push(byte);
    }
    Ok(bytes)
}

/// Parse hex pattern with wildcard support. Returns None for wildcard bytes ("??").
fn parse_hex_pattern(s: &str) -> Result<Vec<Option<u8>>, String> {
    let tokens: Vec<&str> = s.split_whitespace().collect();
    if tokens.is_empty() {
        return Err("Empty hex pattern".to_string());
    }

    let mut result = Vec::with_capacity(tokens.len());
    for token in tokens {
        if token == "??" {
            result.push(None);
        } else if token.len() == 2 {
            let byte = u8::from_str_radix(token, 16)
                .map_err(|_| format!("Invalid hex byte: '{}'", token))?;
            result.push(Some(byte));
        } else {
            return Err(format!(
                "Invalid token '{}': expected 2-digit hex or '??'",
                token
            ));
        }
    }
    Ok(result)
}

fn search_bytes(data: &[u8], pattern: &[u8], max_results: usize) -> Vec<usize> {
    let mut results = Vec::new();
    let finder = memmem::Finder::new(pattern);
    let mut start = 0;

    while let Some(pos) = finder.find(&data[start..]) {
        results.push(start + pos);
        if results.len() >= max_results {
            break;
        }
        start += pos + 1;
    }
    results
}

fn search_bytes_wildcard(
    data: &[u8],
    pattern: &[Option<u8>],
    max_results: usize,
) -> Vec<usize> {
    if pattern.is_empty() || data.len() < pattern.len() {
        return vec![];
    }

    let mut results = Vec::new();
    let end = data.len() - pattern.len() + 1;

    for i in 0..end {
        let matched = pattern.iter().enumerate().all(|(j, p)| match p {
            Some(b) => data[i + j] == *b,
            None => true,
        });
        if matched {
            results.push(i);
            if results.len() >= max_results {
                break;
            }
        }
    }
    results
}

fn write_bytes_to_file(path: &str, offset: u64, bytes: &[u8]) -> std::io::Result<()> {
    let mut file = fs::OpenOptions::new().write(true).open(path)?;
    file.seek(SeekFrom::Start(offset))?;
    file.write_all(bytes)?;
    file.flush()?;
    Ok(())
}

fn patch_file(
    path: &str,
    offset: u64,
    delete_count: usize,
    insert_data: &[u8],
) -> std::io::Result<()> {
    // Create backup
    let backup_path = format!("{}.bak", path);
    fs::copy(path, &backup_path)?;

    let mut data = fs::read(path)?;

    let offset = offset as usize;
    if offset > data.len() {
        return Err(std::io::Error::new(
            std::io::ErrorKind::InvalidInput,
            format!("Offset {} beyond file size {}", offset, data.len()),
        ));
    }

    // Delete bytes
    let delete_end = (offset + delete_count).min(data.len());
    data.drain(offset..delete_end);

    // Insert bytes
    if !insert_data.is_empty() {
        let insert_at = offset.min(data.len());
        for (i, &byte) in insert_data.iter().enumerate() {
            data.insert(insert_at + i, byte);
        }
    }

    fs::write(path, &data)?;
    Ok(())
}

fn interpret_bytes(data: &[u8], base_offset: u64) -> String {
    let mut output = String::new();
    output.push_str(&format!(
        "Interpretation at offset 0x{:X} ({} byte(s) available):\n\n",
        base_offset,
        data.len()
    ));

    // Raw hex
    let hex: Vec<String> = data.iter().map(|b| format!("{:02x}", b)).collect();
    output.push_str(&format!("Raw: {}\n\n", hex.join(" ")));

    // u8 / i8
    if !data.is_empty() {
        output.push_str(&format!("u8:  {}\n", data[0]));
        output.push_str(&format!("i8:  {}\n", data[0] as i8));
    }

    // u16 / i16
    if data.len() >= 2 {
        let le = u16::from_le_bytes([data[0], data[1]]);
        let be = u16::from_be_bytes([data[0], data[1]]);
        output.push_str(&format!("u16: {} (LE) / {} (BE)\n", le, be));
        output.push_str(&format!("i16: {} (LE) / {} (BE)\n", le as i16, be as i16));
    }

    // u32 / i32 / f32
    if data.len() >= 4 {
        let le = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        let be = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
        output.push_str(&format!("u32: {} (LE) / {} (BE)\n", le, be));
        output.push_str(&format!("i32: {} (LE) / {} (BE)\n", le as i32, be as i32));
        let fle = f32::from_le_bytes([data[0], data[1], data[2], data[3]]);
        let fbe = f32::from_be_bytes([data[0], data[1], data[2], data[3]]);
        output.push_str(&format!("f32: {} (LE) / {} (BE)\n", fle, fbe));
    }

    // u64 / i64 / f64
    if data.len() >= 8 {
        let bytes: [u8; 8] = data[..8].try_into().unwrap();
        let le = u64::from_le_bytes(bytes);
        let be = u64::from_be_bytes(bytes);
        output.push_str(&format!("u64: {} (LE) / {} (BE)\n", le, be));
        output.push_str(&format!("i64: {} (LE) / {} (BE)\n", le as i64, be as i64));
        let fle = f64::from_le_bytes(bytes);
        let fbe = f64::from_be_bytes(bytes);
        output.push_str(&format!("f64: {} (LE) / {} (BE)\n", fle, fbe));
    }

    // ASCII
    output.push_str("\nASCII: ");
    for &b in data {
        if b.is_ascii_graphic() || b == b' ' {
            output.push(b as char);
        } else {
            output.push('.');
        }
    }
    output.push('\n');

    // UTF-8
    if let Ok(s) = std::str::from_utf8(data) {
        output.push_str(&format!("UTF-8: {}\n", s));
    }

    output
}

fn detect_magic(data: &[u8]) -> Option<&'static str> {
    if data.len() < 4 {
        return None;
    }

    match &data[..4] {
        [0x7f, b'E', b'L', b'F'] => Some("ELF executable"),
        [b'M', b'Z', ..] => Some("PE/DOS executable"),
        [0xfe, 0xed, 0xfa, 0xce] => Some("Mach-O (32-bit)"),
        [0xfe, 0xed, 0xfa, 0xcf] => Some("Mach-O (64-bit)"),
        [0xce, 0xfa, 0xed, 0xfe] => Some("Mach-O (32-bit, reversed)"),
        [0xcf, 0xfa, 0xed, 0xfe] => Some("Mach-O (64-bit, reversed)"),
        [0xca, 0xfe, 0xba, 0xbe] => Some("Mach-O Universal / Java class"),
        [b'P', b'K', 0x03, 0x04] => Some("ZIP archive"),
        [b'P', b'K', 0x05, 0x06] => Some("ZIP archive (empty)"),
        [0x1f, 0x8b, ..] => Some("gzip compressed"),
        [0x89, b'P', b'N', b'G'] => Some("PNG image"),
        [0xff, 0xd8, 0xff, ..] => Some("JPEG image"),
        [b'G', b'I', b'F', b'8'] => Some("GIF image"),
        [b'R', b'I', b'F', b'F'] => Some("RIFF (WAV/AVI/WebP)"),
        [b'%', b'P', b'D', b'F'] => Some("PDF document"),
        [0x00, 0x00, 0x01, 0x00] => Some("ICO image"),
        [b'd', b'e', b'x', b'\n'] => Some("Android DEX"),
        [0x50, 0x4b, ..] if data.len() >= 4 => Some("ZIP-based archive"),
        _ => {
            if data.len() >= 8 && &data[4..8] == b"ftyp" {
                Some("MP4/MOV video")
            } else {
                None
            }
        }
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::NamedTempFile;

    #[test]
    fn test_format_hex_dump_basic() {
        let data: Vec<u8> = (0x00..=0x1f).collect();
        let dump = format_hex_dump(&data, 0);
        assert!(dump.contains("00000000"));
        assert!(dump.contains("00 01 02 03"));
        assert!(dump.contains("10 11 12 13"));
    }

    #[test]
    fn test_format_hex_dump_with_offset() {
        let data = vec![0x41, 0x42, 0x43]; // "ABC"
        let dump = format_hex_dump(&data, 0x100);
        assert!(dump.starts_with("00000100"));
        assert!(dump.contains("41 42 43"));
        assert!(dump.contains("ABC"));
    }

    #[test]
    fn test_format_hex_dump_non_printable() {
        let data = vec![0x00, 0x41, 0x7f, 0xff];
        let dump = format_hex_dump(&data, 0);
        // Non-printable replaced with '.'
        assert!(dump.contains(".A.."));
    }

    #[test]
    fn test_parse_hex_string_spaced() {
        let result = parse_hex_string("48 65 6c 6c 6f").unwrap();
        assert_eq!(result, b"Hello");
    }

    #[test]
    fn test_parse_hex_string_no_spaces() {
        let result = parse_hex_string("48656c6c6f").unwrap();
        assert_eq!(result, b"Hello");
    }

    #[test]
    fn test_parse_hex_string_empty() {
        assert!(parse_hex_string("").is_err());
    }

    #[test]
    fn test_parse_hex_string_odd_length() {
        assert!(parse_hex_string("4865f").is_err());
    }

    #[test]
    fn test_parse_hex_string_invalid() {
        assert!(parse_hex_string("ZZ").is_err());
    }

    #[test]
    fn test_parse_hex_pattern_with_wildcard() {
        let result = parse_hex_pattern("4d 5a ?? 00").unwrap();
        assert_eq!(result, vec![Some(0x4d), Some(0x5a), None, Some(0x00)]);
    }

    #[test]
    fn test_search_bytes_basic() {
        let data = b"Hello World Hello Rust";
        let results = search_bytes(data, b"Hello", 10);
        assert_eq!(results, vec![0, 12]);
    }

    #[test]
    fn test_search_bytes_max_results() {
        let data = b"aaa";
        let results = search_bytes(data, b"a", 2);
        assert_eq!(results.len(), 2);
    }

    #[test]
    fn test_search_bytes_not_found() {
        let data = b"Hello World";
        let results = search_bytes(data, b"xyz", 10);
        assert!(results.is_empty());
    }

    #[test]
    fn test_search_bytes_wildcard() {
        let data = vec![0x4d, 0x5a, 0x90, 0x00, 0x03, 0x4d, 0x5a, 0xff, 0x00];
        let pattern = vec![Some(0x4d), Some(0x5a), None, Some(0x00)];
        let results = search_bytes_wildcard(&data, &pattern, 10);
        assert_eq!(results, vec![0, 5]);
    }

    #[test]
    fn test_interpret_bytes_full() {
        let data = vec![0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08];
        let output = interpret_bytes(&data, 0);
        assert!(output.contains("u8:  1"));
        assert!(output.contains("u16:"));
        assert!(output.contains("u32:"));
        assert!(output.contains("u64:"));
        assert!(output.contains("f32:"));
        assert!(output.contains("f64:"));
        assert!(output.contains("(LE)"));
        assert!(output.contains("(BE)"));
    }

    #[test]
    fn test_interpret_bytes_partial() {
        let data = vec![0x41]; // Just one byte 'A'
        let output = interpret_bytes(&data, 0);
        assert!(output.contains("u8:  65"));
        assert!(output.contains("i8:  65"));
        // Should not contain u16+
        assert!(!output.contains("u16:"));
    }

    #[test]
    fn test_read_write_file() {
        let mut tmp = NamedTempFile::new().unwrap();
        tmp.write_all(b"Hello World!").unwrap();
        tmp.flush().unwrap();
        let path = tmp.path().to_str().unwrap();

        // Read
        let data = read_bytes_from_file(path, 0, 5).unwrap();
        assert_eq!(data, b"Hello");

        // Read with offset
        let data = read_bytes_from_file(path, 6, 5).unwrap();
        assert_eq!(data, b"World");

        // Read beyond EOF
        let data = read_bytes_from_file(path, 100, 5).unwrap();
        assert!(data.is_empty());

        // Write
        write_bytes_to_file(path, 0, b"Jello").unwrap();
        let data = read_bytes_from_file(path, 0, 12).unwrap();
        assert_eq!(data, b"Jello World!");
    }

    #[test]
    fn test_patch_file_delete() {
        let mut tmp = NamedTempFile::new().unwrap();
        tmp.write_all(b"Hello World!").unwrap();
        tmp.flush().unwrap();
        let path = tmp.path().to_str().unwrap();

        patch_file(path, 5, 1, &[]).unwrap();
        let data = fs::read(path).unwrap();
        assert_eq!(data, b"HelloWorld!");

        // Check backup
        let backup = fs::read(format!("{}.bak", path)).unwrap();
        assert_eq!(backup, b"Hello World!");

        // Clean up backup
        let _ = fs::remove_file(format!("{}.bak", path));
    }

    #[test]
    fn test_patch_file_insert() {
        let mut tmp = NamedTempFile::new().unwrap();
        tmp.write_all(b"HelloWorld!").unwrap();
        tmp.flush().unwrap();
        let path = tmp.path().to_str().unwrap();

        patch_file(path, 5, 0, b" ").unwrap();
        let data = fs::read(path).unwrap();
        assert_eq!(data, b"Hello World!");

        let _ = fs::remove_file(format!("{}.bak", path));
    }

    #[test]
    fn test_patch_file_replace() {
        let mut tmp = NamedTempFile::new().unwrap();
        tmp.write_all(b"Hello World!").unwrap();
        tmp.flush().unwrap();
        let path = tmp.path().to_str().unwrap();

        // Replace "World" with "Rust"
        patch_file(path, 6, 5, b"Rust").unwrap();
        let data = fs::read(path).unwrap();
        assert_eq!(data, b"Hello Rust!");

        let _ = fs::remove_file(format!("{}.bak", path));
    }

    #[test]
    fn test_detect_file_type_via_command() {
        // Create a temp file with PNG-like header
        let mut tmp = NamedTempFile::new().unwrap();
        tmp.write_all(b"Hello, this is a plain text file.\n").unwrap();
        tmp.flush().unwrap();
        let path = tmp.path().to_str().unwrap();

        let data = read_bytes_from_file(path, 0, 1024).unwrap();
        let result = std::process::Command::new("file")
            .args(["-b", "-"])
            .stdin(std::process::Stdio::piped())
            .stdout(std::process::Stdio::piped())
            .stderr(std::process::Stdio::piped())
            .spawn()
            .and_then(|mut child| {
                if let Some(ref mut stdin) = child.stdin {
                    let _ = stdin.write_all(&data);
                }
                drop(child.stdin.take());
                child.wait_with_output()
            });

        match result {
            Ok(output) if output.status.success() => {
                let file_type = String::from_utf8_lossy(&output.stdout).trim().to_string();
                assert!(
                    file_type.to_lowercase().contains("text"),
                    "Expected 'text' in file type, got: {}",
                    file_type
                );
            }
            Ok(_) => panic!("file command failed"),
            Err(e) => {
                // Skip test if `file` command is not available
                eprintln!("Skipping test: file command not available: {}", e);
            }
        }
    }

    #[test]
    fn test_detect_magic() {
        assert_eq!(detect_magic(&[0x7f, b'E', b'L', b'F']), Some("ELF executable"));
        assert_eq!(detect_magic(&[0x89, b'P', b'N', b'G']), Some("PNG image"));
        assert_eq!(detect_magic(&[0xcf, 0xfa, 0xed, 0xfe]), Some("Mach-O (64-bit, reversed)"));
        assert_eq!(detect_magic(&[0x00, 0x00, 0x00]), None); // too short
    }
}
