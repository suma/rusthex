use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Returns the Unix domain socket path for IPC communication.
pub fn socket_path() -> PathBuf {
    std::env::temp_dir().join("rusthex.sock")
}

/// Send an IPC request to the GUI and return the response.
/// Returns `None` if the GUI is not running (connection failed).
#[cfg(unix)]
pub fn send_request(request: &IpcRequest) -> Option<IpcResponse> {
    use std::io::{BufRead, BufReader, Write};
    use std::os::unix::net::UnixStream;
    use std::time::Duration;

    let path = socket_path();
    let mut stream = UnixStream::connect(&path).ok()?;
    stream.set_read_timeout(Some(Duration::from_secs(10))).ok()?;
    stream.set_write_timeout(Some(Duration::from_secs(5))).ok()?;

    let json = serde_json::to_string(request).ok()?;
    stream.write_all(json.as_bytes()).ok()?;
    stream.write_all(b"\n").ok()?;
    stream.flush().ok()?;

    // Shutdown write half to signal end of request
    stream.shutdown(std::net::Shutdown::Write).ok()?;

    let reader = BufReader::new(&stream);
    let mut line = String::new();
    let mut buf_reader = reader;
    buf_reader.read_line(&mut line).ok()?;

    if line.trim().is_empty() {
        return None;
    }

    serde_json::from_str(line.trim()).ok()
}

#[cfg(not(unix))]
pub fn send_request(_request: &IpcRequest) -> Option<IpcResponse> {
    None
}

// ---------------------------------------------------------------------------
// Protocol types
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "method", content = "params")]
pub enum IpcRequest {
    Ping,
    ListTabs,
    ReadBytes {
        path: String,
        offset: u64,
        length: usize,
    },
    WriteBytes {
        path: String,
        offset: u64,
        #[serde(with = "hex_bytes")]
        data: Vec<u8>,
    },
    InsertBytes {
        path: String,
        offset: u64,
        #[serde(with = "hex_bytes")]
        data: Vec<u8>,
    },
    DeleteBytes {
        path: String,
        offset: u64,
        count: usize,
    },
    SearchBytes {
        path: String,
        pattern: String,
        mode: Option<String>,
        max_results: Option<usize>,
    },
    FileInfo {
        path: String,
    },
    InterpretBytes {
        path: String,
        offset: u64,
    },
    Goto {
        path: String,
        offset: u64,
    },
    Undo {
        path: String,
    },
    Redo {
        path: String,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IpcResponse {
    pub ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<IpcResult>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub error: Option<String>,
}

impl IpcResponse {
    pub fn success(result: IpcResult) -> Self {
        Self {
            ok: true,
            result: Some(result),
            error: None,
        }
    }

    pub fn error(msg: impl Into<String>) -> Self {
        Self {
            ok: false,
            result: None,
            error: Some(msg.into()),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum IpcResult {
    Pong,
    Tabs {
        tabs: Vec<TabInfo>,
    },
    Bytes {
        data: String,
        raw_hex: String,
        length: usize,
    },
    WriteOk {
        bytes_written: usize,
    },
    InsertOk {
        bytes_inserted: usize,
    },
    DeleteOk {
        bytes_deleted: usize,
    },
    SearchResults {
        matches: Vec<u64>,
        total: usize,
    },
    FileInfo {
        path: String,
        size: u64,
        has_unsaved_changes: bool,
        cursor_position: u64,
    },
    Interpretation {
        text: String,
    },
    GotoOk {
        offset: u64,
    },
    UndoOk {
        offset: Option<u64>,
    },
    RedoOk {
        offset: Option<u64>,
    },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TabInfo {
    pub index: usize,
    pub path: Option<String>,
    pub name: String,
    pub size: u64,
    pub has_unsaved_changes: bool,
    pub is_active: bool,
}

// ---------------------------------------------------------------------------
// Hex bytes serialization helper (Vec<u8> <-> hex string)
// ---------------------------------------------------------------------------

mod hex_bytes {
    use serde::{self, Deserialize, Deserializer, Serializer};

    pub fn serialize<S>(bytes: &Vec<u8>, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let hex: String = bytes.iter().map(|b| format!("{:02x}", b)).collect();
        serializer.serialize_str(&hex)
    }

    pub fn deserialize<'de, D>(deserializer: D) -> Result<Vec<u8>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        let s = s.replace(' ', "");
        if s.len() % 2 != 0 {
            return Err(serde::de::Error::custom("hex string must have even length"));
        }
        (0..s.len())
            .step_by(2)
            .map(|i| {
                u8::from_str_radix(&s[i..i + 2], 16)
                    .map_err(|_| serde::de::Error::custom("invalid hex byte"))
            })
            .collect()
    }
}

// ---------------------------------------------------------------------------
// Tests
// ---------------------------------------------------------------------------

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_socket_path() {
        let path = socket_path();
        assert!(path.to_string_lossy().contains("rusthex.sock"));
    }

    #[test]
    fn test_request_ping_serialization() {
        let req = IpcRequest::Ping;
        let json = serde_json::to_string(&req).unwrap();
        assert_eq!(json, r#"{"method":"Ping"}"#);

        let deserialized: IpcRequest = serde_json::from_str(&json).unwrap();
        assert!(matches!(deserialized, IpcRequest::Ping));
    }

    #[test]
    fn test_request_list_tabs_serialization() {
        let req = IpcRequest::ListTabs;
        let json = serde_json::to_string(&req).unwrap();
        assert_eq!(json, r#"{"method":"ListTabs"}"#);
    }

    #[test]
    fn test_request_read_bytes_serialization() {
        let req = IpcRequest::ReadBytes {
            path: "/tmp/test.bin".into(),
            offset: 0x100,
            length: 64,
        };
        let json = serde_json::to_string(&req).unwrap();
        let deserialized: IpcRequest = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcRequest::ReadBytes {
                path,
                offset,
                length,
            } => {
                assert_eq!(path, "/tmp/test.bin");
                assert_eq!(offset, 0x100);
                assert_eq!(length, 64);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_request_write_bytes_serialization() {
        let req = IpcRequest::WriteBytes {
            path: "/tmp/test.bin".into(),
            offset: 0,
            data: vec![0xDE, 0xAD, 0xBE, 0xEF],
        };
        let json = serde_json::to_string(&req).unwrap();
        assert!(json.contains("deadbeef"));

        let deserialized: IpcRequest = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcRequest::WriteBytes { data, .. } => {
                assert_eq!(data, vec![0xDE, 0xAD, 0xBE, 0xEF]);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_request_goto_serialization() {
        let req = IpcRequest::Goto {
            path: "/tmp/test.bin".into(),
            offset: 0xFF00,
        };
        let json = serde_json::to_string(&req).unwrap();
        let deserialized: IpcRequest = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcRequest::Goto { path, offset } => {
                assert_eq!(path, "/tmp/test.bin");
                assert_eq!(offset, 0xFF00);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_response_success_serialization() {
        let resp = IpcResponse::success(IpcResult::Pong);
        let json = serde_json::to_string(&resp).unwrap();
        let deserialized: IpcResponse = serde_json::from_str(&json).unwrap();
        assert!(deserialized.ok);
        assert!(matches!(deserialized.result, Some(IpcResult::Pong)));
        assert!(deserialized.error.is_none());
    }

    #[test]
    fn test_response_error_serialization() {
        let resp = IpcResponse::error("file not open in GUI");
        let json = serde_json::to_string(&resp).unwrap();
        let deserialized: IpcResponse = serde_json::from_str(&json).unwrap();
        assert!(!deserialized.ok);
        assert!(deserialized.result.is_none());
        assert_eq!(deserialized.error.as_deref(), Some("file not open in GUI"));
    }

    #[test]
    fn test_result_tabs_serialization() {
        let result = IpcResult::Tabs {
            tabs: vec![TabInfo {
                index: 0,
                path: Some("/tmp/test.bin".into()),
                name: "test.bin".into(),
                size: 1024,
                has_unsaved_changes: false,
                is_active: true,
            }],
        };
        let json = serde_json::to_string(&result).unwrap();
        let deserialized: IpcResult = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcResult::Tabs { tabs } => {
                assert_eq!(tabs.len(), 1);
                assert_eq!(tabs[0].name, "test.bin");
                assert!(tabs[0].is_active);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_result_bytes_serialization() {
        let result = IpcResult::Bytes {
            data: "00000000  48 65 6c 6c 6f  |Hello|".into(),
            raw_hex: "48656c6c6f".into(),
            length: 5,
        };
        let json = serde_json::to_string(&result).unwrap();
        let deserialized: IpcResult = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcResult::Bytes {
                data, raw_hex, length, ..
            } => {
                assert_eq!(length, 5);
                assert!(data.contains("Hello"));
                assert_eq!(raw_hex, "48656c6c6f");
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_result_search_serialization() {
        let result = IpcResult::SearchResults {
            matches: vec![0x100, 0x200, 0x300],
            total: 3,
        };
        let json = serde_json::to_string(&result).unwrap();
        let deserialized: IpcResult = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcResult::SearchResults { matches, total } => {
                assert_eq!(matches, vec![0x100, 0x200, 0x300]);
                assert_eq!(total, 3);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_result_file_info_serialization() {
        let result = IpcResult::FileInfo {
            path: "/tmp/test.bin".into(),
            size: 4096,
            has_unsaved_changes: true,
            cursor_position: 0x42,
        };
        let json = serde_json::to_string(&result).unwrap();
        let deserialized: IpcResult = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcResult::FileInfo {
                size,
                has_unsaved_changes,
                cursor_position,
                ..
            } => {
                assert_eq!(size, 4096);
                assert!(has_unsaved_changes);
                assert_eq!(cursor_position, 0x42);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_result_undo_redo_serialization() {
        let undo = IpcResult::UndoOk {
            offset: Some(0x100),
        };
        let json = serde_json::to_string(&undo).unwrap();
        let deserialized: IpcResult = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcResult::UndoOk { offset } => assert_eq!(offset, Some(0x100)),
            _ => panic!("wrong variant"),
        }

        let redo = IpcResult::RedoOk { offset: None };
        let json = serde_json::to_string(&redo).unwrap();
        let deserialized: IpcResult = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcResult::RedoOk { offset } => assert_eq!(offset, None),
            _ => panic!("wrong variant"),
        }
    }

    #[cfg(unix)]
    #[test]
    fn test_send_request_no_server() {
        // With no server running, send_request should return None
        let result = send_request(&IpcRequest::Ping);
        assert!(result.is_none());
    }

    #[test]
    fn test_hex_bytes_roundtrip() {
        let req = IpcRequest::InsertBytes {
            path: "/tmp/test.bin".into(),
            offset: 0,
            data: vec![0x00, 0xFF, 0x42, 0xAB],
        };
        let json = serde_json::to_string(&req).unwrap();
        assert!(json.contains("00ff42ab"));

        let deserialized: IpcRequest = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcRequest::InsertBytes { data, .. } => {
                assert_eq!(data, vec![0x00, 0xFF, 0x42, 0xAB]);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_delete_bytes_serialization() {
        let req = IpcRequest::DeleteBytes {
            path: "/tmp/test.bin".into(),
            offset: 0x10,
            count: 4,
        };
        let json = serde_json::to_string(&req).unwrap();
        let deserialized: IpcRequest = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcRequest::DeleteBytes {
                path,
                offset,
                count,
            } => {
                assert_eq!(path, "/tmp/test.bin");
                assert_eq!(offset, 0x10);
                assert_eq!(count, 4);
            }
            _ => panic!("wrong variant"),
        }
    }

    #[test]
    fn test_search_bytes_serialization() {
        let req = IpcRequest::SearchBytes {
            path: "/tmp/test.bin".into(),
            pattern: "DEADBEEF".into(),
            mode: Some("hex".into()),
            max_results: Some(100),
        };
        let json = serde_json::to_string(&req).unwrap();
        let deserialized: IpcRequest = serde_json::from_str(&json).unwrap();
        match deserialized {
            IpcRequest::SearchBytes {
                pattern,
                mode,
                max_results,
                ..
            } => {
                assert_eq!(pattern, "DEADBEEF");
                assert_eq!(mode.as_deref(), Some("hex"));
                assert_eq!(max_results, Some(100));
            }
            _ => panic!("wrong variant"),
        }
    }
}
