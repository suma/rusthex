//! IPC server for communication between rusthex GUI and rusthex-mcp.
//!
//! Architecture:
//! - std::thread runs UnixListener, spawns a thread per connection
//! - Each connection thread reads one JSON line, sends via mpsc, waits for response
//! - gpui cx.spawn() polls the mpsc receiver every 50ms
//! - Requests are dispatched to HexEditor methods on the main thread

#[cfg(unix)]
mod server {
    use crate::HexEditor;
    use gpui::{Context, Entity, Timer};
    use rusthex_ipc::{IpcRequest, IpcResponse, IpcResult, TabInfo, socket_path};
    use std::io::{BufRead, BufReader, Write};
    use std::os::unix::net::UnixListener;
    use std::sync::mpsc;
    use std::time::Duration;

    /// Message sent from connection thread to the gpui polling task.
    struct IpcMessage {
        request: IpcRequest,
        responder: mpsc::Sender<IpcResponse>,
    }

    /// Start the IPC server: listener thread + gpui polling task.
    pub fn start_ipc_server(editor: &Entity<HexEditor>, cx: &mut Context<HexEditor>) {
        let (tx, rx) = mpsc::channel::<IpcMessage>();

        // Spawn listener thread
        std::thread::spawn(move || {
            let path = socket_path();

            // Remove stale socket file
            let _ = std::fs::remove_file(&path);

            let listener = match UnixListener::bind(&path) {
                Ok(l) => l,
                Err(e) => {
                    eprintln!("IPC: failed to bind {}: {}", path.display(), e);
                    return;
                }
            };

            for stream in listener.incoming() {
                let stream = match stream {
                    Ok(s) => s,
                    Err(_) => continue,
                };

                let tx = tx.clone();
                std::thread::spawn(move || {
                    let _ = stream.set_read_timeout(Some(Duration::from_secs(5)));
                    let _ = stream.set_write_timeout(Some(Duration::from_secs(5)));

                    let mut reader = BufReader::new(&stream);
                    let mut line = String::new();
                    if reader.read_line(&mut line).is_err() || line.trim().is_empty() {
                        return;
                    }

                    let request: IpcRequest = match serde_json::from_str(line.trim()) {
                        Ok(r) => r,
                        Err(e) => {
                            let resp = IpcResponse::error(format!("invalid JSON: {}", e));
                            let json = serde_json::to_string(&resp).unwrap_or_default();
                            let mut writer = stream;
                            let _ = writer.write_all(json.as_bytes());
                            let _ = writer.write_all(b"\n");
                            let _ = writer.flush();
                            return;
                        }
                    };

                    // Create oneshot-style channel for the response
                    let (resp_tx, resp_rx) = mpsc::channel::<IpcResponse>();

                    let msg = IpcMessage {
                        request,
                        responder: resp_tx,
                    };

                    if tx.send(msg).is_err() {
                        return;
                    }

                    // Wait for response (10 second timeout)
                    let response = match resp_rx.recv_timeout(Duration::from_secs(10)) {
                        Ok(r) => r,
                        Err(_) => IpcResponse::error("timeout waiting for GUI response"),
                    };

                    let json = serde_json::to_string(&response).unwrap_or_default();
                    let mut writer = stream;
                    let _ = writer.write_all(json.as_bytes());
                    let _ = writer.write_all(b"\n");
                    let _ = writer.flush();
                });
            }
        });

        // Spawn gpui polling task (50ms interval)
        let entity = editor.downgrade();
        cx.spawn(async move |_this, cx| {
            loop {
                Timer::after(Duration::from_millis(50)).await;

                // Try to receive all pending messages
                loop {
                    let msg = match rx.try_recv() {
                        Ok(m) => m,
                        Err(_) => break,
                    };

                    let request = msg.request;
                    let responder = msg.responder;

                    let response = match entity.update(cx, |editor, cx| {
                        let resp = editor.handle_ipc_request(&request);
                        cx.notify();
                        resp
                    }) {
                        Ok(r) => r,
                        Err(_) => IpcResponse::error("editor entity dropped"),
                    };

                    let _ = responder.send(response);
                }
            }
        })
        .detach();
    }

    impl HexEditor {
        /// Dispatch an IPC request to the appropriate handler.
        pub fn handle_ipc_request(&mut self, request: &IpcRequest) -> IpcResponse {
            match request {
                IpcRequest::Ping => IpcResponse::success(IpcResult::Pong),

                IpcRequest::ListTabs => {
                    let tabs: Vec<TabInfo> = self
                        .tabs
                        .iter()
                        .enumerate()
                        .map(|(i, tab)| TabInfo {
                            index: i,
                            path: tab.document.file_path().map(|p| p.to_string_lossy().into()),
                            name: tab.display_name(),
                            size: tab.document.len() as u64,
                            has_unsaved_changes: tab.document.has_unsaved_changes(),
                            is_active: i == self.active_tab,
                        })
                        .collect();
                    IpcResponse::success(IpcResult::Tabs { tabs })
                }

                IpcRequest::ReadBytes {
                    path,
                    offset,
                    length,
                } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        let doc = &self.tabs[idx].document;
                        let offset = *offset as usize;
                        let length = (*length).min(65536);
                        let end = (offset + length).min(doc.len());

                        if offset >= doc.len() {
                            return IpcResponse::success(IpcResult::Bytes {
                                data: "No data at specified offset (beyond end of file).".into(),
                                raw_hex: String::new(),
                                length: 0,
                            });
                        }

                        let bytes: Vec<u8> = (offset..end)
                            .filter_map(|i| doc.get_byte(i))
                            .collect();

                        let raw_hex: String =
                            bytes.iter().map(|b| format!("{:02x}", b)).collect();
                        let data = format_hex_dump(&bytes, offset as u64);

                        IpcResponse::success(IpcResult::Bytes {
                            data,
                            raw_hex,
                            length: bytes.len(),
                        })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },

                IpcRequest::WriteBytes {
                    path,
                    offset,
                    data,
                } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        let offset = *offset as usize;
                        let mut written = 0;
                        for (i, &byte) in data.iter().enumerate() {
                            if let Err(e) = self.tabs[idx].document.set_byte(offset + i, byte) {
                                return IpcResponse::error(format!(
                                    "write error at offset 0x{:X}: {}",
                                    offset + i,
                                    e
                                ));
                            }
                            written += 1;
                        }
                        self.invalidate_render_cache();
                        IpcResponse::success(IpcResult::WriteOk {
                            bytes_written: written,
                        })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },

                IpcRequest::InsertBytes {
                    path,
                    offset,
                    data,
                } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        let offset = *offset as usize;
                        if let Err(e) = self.tabs[idx].document.insert_bytes(offset, data) {
                            return IpcResponse::error(format!("insert error: {}", e));
                        }
                        self.invalidate_render_cache();
                        IpcResponse::success(IpcResult::InsertOk {
                            bytes_inserted: data.len(),
                        })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },

                IpcRequest::DeleteBytes {
                    path,
                    offset,
                    count,
                } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        let offset = *offset as usize;
                        if let Err(e) = self.tabs[idx].document.delete_bytes(offset, *count) {
                            return IpcResponse::error(format!("delete error: {}", e));
                        }
                        self.invalidate_render_cache();
                        IpcResponse::success(IpcResult::DeleteOk {
                            bytes_deleted: *count,
                        })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },

                IpcRequest::SearchBytes {
                    path,
                    pattern,
                    mode,
                    max_results,
                } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        let doc = &self.tabs[idx].document;
                        let data = doc.to_vec();
                        let max = max_results.unwrap_or(20).min(1000);
                        let mode_str = mode.as_deref().unwrap_or("hex");

                        let results = match mode_str {
                            "ascii" => {
                                let pat = pattern.as_bytes();
                                search_bytes_simple(&data, pat, max)
                            }
                            _ => {
                                match parse_hex_pattern(pattern) {
                                    Ok(parsed) => {
                                        if parsed.iter().any(|b| b.is_none()) {
                                            search_bytes_wildcard(&data, &parsed, max)
                                        } else {
                                            let concrete: Vec<u8> =
                                                parsed.into_iter().map(|b| b.unwrap()).collect();
                                            search_bytes_simple(&data, &concrete, max)
                                        }
                                    }
                                    Err(e) => return IpcResponse::error(format!("invalid pattern: {}", e)),
                                }
                            }
                        };

                        let matches: Vec<u64> = results.into_iter().map(|i| i as u64).collect();
                        let total = matches.len();
                        IpcResponse::success(IpcResult::SearchResults { matches, total })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },

                IpcRequest::FileInfo { path } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        let tab = &self.tabs[idx];
                        let doc = &tab.document;
                        let file_path = doc
                            .file_path()
                            .map(|p| p.to_string_lossy().into_owned())
                            .unwrap_or_else(|| path.clone());
                        IpcResponse::success(IpcResult::FileInfo {
                            path: file_path,
                            size: doc.len() as u64,
                            has_unsaved_changes: doc.has_unsaved_changes(),
                            cursor_position: tab.cursor_position as u64,
                        })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },

                IpcRequest::InterpretBytes { path, offset } => {
                    match self.find_tab_by_path(path) {
                        Some(idx) => {
                            let doc = &self.tabs[idx].document;
                            let offset = *offset as usize;
                            let length = 8.min(doc.len().saturating_sub(offset));

                            if length == 0 {
                                return IpcResponse::error("offset beyond end of file");
                            }

                            let bytes: Vec<u8> = (offset..offset + length)
                                .filter_map(|i| doc.get_byte(i))
                                .collect();
                            let text = interpret_bytes_inline(&bytes, offset as u64);
                            IpcResponse::success(IpcResult::Interpretation { text })
                        }
                        None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                    }
                }

                IpcRequest::Goto { path, offset } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        self.active_tab = idx;
                        let doc_len = self.tabs[idx].document.len();
                        let offset = (*offset as usize).min(doc_len.saturating_sub(1));
                        self.tabs[idx].cursor_position = offset;
                        self.ensure_cursor_visible_by_row();
                        IpcResponse::success(IpcResult::GotoOk {
                            offset: offset as u64,
                        })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },

                IpcRequest::Undo { path } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        let result = self.tabs[idx].document.undo();
                        if let Some(off) = result {
                            self.tabs[idx].cursor_position = off;
                        }
                        self.invalidate_render_cache();
                        IpcResponse::success(IpcResult::UndoOk {
                            offset: result.map(|o| o as u64),
                        })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },

                IpcRequest::Redo { path } => match self.find_tab_by_path(path) {
                    Some(idx) => {
                        let result = self.tabs[idx].document.redo();
                        if let Some(off) = result {
                            self.tabs[idx].cursor_position = off;
                        }
                        self.invalidate_render_cache();
                        IpcResponse::success(IpcResult::RedoOk {
                            offset: result.map(|o| o as u64),
                        })
                    }
                    None => IpcResponse::error(format!("file not open in GUI: {}", path)),
                },
            }
        }

        /// Find a tab by file path, using canonicalized path comparison.
        fn find_tab_by_path(&self, path: &str) -> Option<usize> {
            let target = std::fs::canonicalize(path)
                .unwrap_or_else(|_| std::path::PathBuf::from(path));

            self.tabs.iter().position(|tab| {
                tab.document.file_path().is_some_and(|p| {
                    let canonical = std::fs::canonicalize(p).unwrap_or_else(|_| p.clone());
                    canonical == target
                })
            })
        }
    }

    // -----------------------------------------------------------------------
    // Inline helpers (duplicated from rusthex-mcp to avoid cross-crate dep)
    // -----------------------------------------------------------------------

    fn format_hex_dump(data: &[u8], base_offset: u64) -> String {
        let mut output = String::new();
        for (i, chunk) in data.chunks(16).enumerate() {
            let addr = base_offset + (i * 16) as u64;
            output.push_str(&format!("{:08X}  ", addr));

            for (j, byte) in chunk.iter().enumerate() {
                if j == 8 {
                    output.push(' ');
                }
                output.push_str(&format!("{:02x} ", byte));
            }

            if chunk.len() < 16 {
                let missing = 16 - chunk.len();
                for j in 0..missing {
                    if chunk.len() + j == 8 {
                        output.push(' ');
                    }
                    output.push_str("   ");
                }
            }

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

    fn interpret_bytes_inline(data: &[u8], base_offset: u64) -> String {
        let mut output = String::new();
        output.push_str(&format!(
            "Interpretation at offset 0x{:X} ({} byte(s) available):\n\n",
            base_offset,
            data.len()
        ));

        let hex: Vec<String> = data.iter().map(|b| format!("{:02x}", b)).collect();
        output.push_str(&format!("Raw: {}\n\n", hex.join(" ")));

        if !data.is_empty() {
            output.push_str(&format!("u8:  {}\n", data[0]));
            output.push_str(&format!("i8:  {}\n", data[0] as i8));
        }
        if data.len() >= 2 {
            let le = u16::from_le_bytes([data[0], data[1]]);
            let be = u16::from_be_bytes([data[0], data[1]]);
            output.push_str(&format!("u16: {} (LE) / {} (BE)\n", le, be));
            output.push_str(&format!("i16: {} (LE) / {} (BE)\n", le as i16, be as i16));
        }
        if data.len() >= 4 {
            let le = u32::from_le_bytes([data[0], data[1], data[2], data[3]]);
            let be = u32::from_be_bytes([data[0], data[1], data[2], data[3]]);
            output.push_str(&format!("u32: {} (LE) / {} (BE)\n", le, be));
            output.push_str(&format!("i32: {} (LE) / {} (BE)\n", le as i32, be as i32));
            let fle = f32::from_le_bytes([data[0], data[1], data[2], data[3]]);
            let fbe = f32::from_be_bytes([data[0], data[1], data[2], data[3]]);
            output.push_str(&format!("f32: {} (LE) / {} (BE)\n", fle, fbe));
        }
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

        output.push_str("\nASCII: ");
        for &b in data {
            if b.is_ascii_graphic() || b == b' ' {
                output.push(b as char);
            } else {
                output.push('.');
            }
        }
        output.push('\n');

        if let Ok(s) = std::str::from_utf8(data) {
            output.push_str(&format!("UTF-8: {}\n", s));
        }

        output
    }

    fn parse_hex_pattern(s: &str) -> Result<Vec<Option<u8>>, String> {
        let tokens: Vec<&str> = s.split_whitespace().collect();
        if tokens.is_empty() {
            return Err("empty hex pattern".into());
        }
        let mut result = Vec::with_capacity(tokens.len());
        for token in tokens {
            if token == "??" {
                result.push(None);
            } else if token.len() == 2 {
                let byte = u8::from_str_radix(token, 16)
                    .map_err(|_| format!("invalid hex byte: '{}'", token))?;
                result.push(Some(byte));
            } else {
                return Err(format!(
                    "invalid token '{}': expected 2-digit hex or '??'",
                    token
                ));
            }
        }
        Ok(result)
    }

    fn search_bytes_simple(data: &[u8], pattern: &[u8], max_results: usize) -> Vec<usize> {
        let mut results = Vec::new();
        let finder = memchr::memmem::Finder::new(pattern);
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

    /// Remove the socket file on shutdown.
    pub fn cleanup_socket() {
        let _ = std::fs::remove_file(socket_path());
    }
}

#[cfg(unix)]
pub use server::{cleanup_socket, start_ipc_server};

// No-op stubs for non-Unix platforms
#[cfg(not(unix))]
pub fn start_ipc_server(
    _editor: &gpui::Entity<crate::HexEditor>,
    _cx: &mut gpui::Context<crate::HexEditor>,
) {
}

#[cfg(not(unix))]
pub fn cleanup_socket() {}
