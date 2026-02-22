//! Log panel for displaying accumulated status messages

use std::collections::VecDeque;
use std::time::Instant;

/// Active tab in the bottom panel
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
pub enum BottomPanelTab {
    #[default]
    Log,
    Info,
}

/// Severity level for log entries
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum LogLevel {
    Info,
    Warning,
    Error,
}

/// A single log entry with level, message, and timestamp
#[derive(Debug, Clone)]
pub struct LogEntry {
    pub level: LogLevel,
    pub message: String,
    pub timestamp: Instant,
}

/// Minimum log panel height in pixels (2 rows)
pub const MIN_HEIGHT: f32 = 38.0;
/// Maximum log panel height in pixels (25 rows)
pub const MAX_HEIGHT: f32 = 475.0;
/// Default log panel height in pixels (3 rows)
pub const DEFAULT_HEIGHT: f32 = 49.0;

/// Scrollable log panel that accumulates status messages
pub struct LogPanel {
    pub entries: VecDeque<LogEntry>,
    pub visible: bool,
    pub max_entries: usize,
    pub start_time: Instant,
    /// Current panel height in pixels
    pub panel_height: f32,
    /// Drag state: (start_y, start_height), None when not dragging
    pub drag_state: Option<(f32, f32)>,
    /// Active tab in the bottom panel
    pub active_tab: BottomPanelTab,
}

impl LogPanel {
    pub fn new() -> Self {
        Self {
            entries: VecDeque::new(),
            visible: true,
            max_entries: 1000,
            start_time: Instant::now(),
            panel_height: DEFAULT_HEIGHT,
            drag_state: None,
            active_tab: BottomPanelTab::default(),
        }
    }

    /// Add a new log entry, evicting the oldest if at capacity
    pub fn push(&mut self, level: LogLevel, message: impl Into<String>) {
        if self.entries.len() >= self.max_entries {
            self.entries.pop_front();
        }
        self.entries.push_back(LogEntry {
            level,
            message: message.into(),
            timestamp: Instant::now(),
        });
    }

    /// Get the most recent log entry, if any
    pub fn latest(&self) -> Option<&LogEntry> {
        self.entries.back()
    }

    /// Clear all log entries
    pub fn clear(&mut self) {
        self.entries.clear();
    }

    /// Format a timestamp as MM:SS relative to start_time
    pub fn format_timestamp(&self, timestamp: Instant) -> String {
        let elapsed = timestamp.duration_since(self.start_time);
        let total_secs = elapsed.as_secs();
        let minutes = total_secs / 60;
        let seconds = total_secs % 60;
        format!("{:02}:{:02}", minutes, seconds)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_panel_is_empty_and_visible() {
        let panel = LogPanel::new();
        assert!(panel.entries.is_empty());
        assert!(panel.visible);
        assert_eq!(panel.max_entries, 1000);
        assert!(panel.latest().is_none());
    }

    #[test]
    fn test_push_and_latest() {
        let mut panel = LogPanel::new();
        panel.push(LogLevel::Info, "Hello");
        assert_eq!(panel.entries.len(), 1);

        let entry = panel.latest().unwrap();
        assert_eq!(entry.level, LogLevel::Info);
        assert_eq!(entry.message, "Hello");
    }

    #[test]
    fn test_latest_returns_most_recent() {
        let mut panel = LogPanel::new();
        panel.push(LogLevel::Info, "First");
        panel.push(LogLevel::Warning, "Second");
        panel.push(LogLevel::Error, "Third");

        let entry = panel.latest().unwrap();
        assert_eq!(entry.message, "Third");
        assert_eq!(entry.level, LogLevel::Error);
    }

    #[test]
    fn test_max_entries_eviction() {
        let mut panel = LogPanel::new();
        panel.max_entries = 3;

        panel.push(LogLevel::Info, "A");
        panel.push(LogLevel::Info, "B");
        panel.push(LogLevel::Info, "C");
        assert_eq!(panel.entries.len(), 3);

        panel.push(LogLevel::Info, "D");
        assert_eq!(panel.entries.len(), 3);
        assert_eq!(panel.entries[0].message, "B");
        assert_eq!(panel.entries[2].message, "D");
    }

    #[test]
    fn test_clear() {
        let mut panel = LogPanel::new();
        panel.push(LogLevel::Info, "A");
        panel.push(LogLevel::Info, "B");
        assert_eq!(panel.entries.len(), 2);

        panel.clear();
        assert!(panel.entries.is_empty());
        assert!(panel.latest().is_none());
    }

    #[test]
    fn test_format_timestamp() {
        let panel = LogPanel::new();
        // Timestamp at start_time should be 00:00
        let ts = panel.start_time;
        assert_eq!(panel.format_timestamp(ts), "00:00");
    }

    #[test]
    fn test_format_timestamp_with_offset() {
        let panel = LogPanel::new();
        // Simulate a timestamp 125 seconds after start
        let ts = panel.start_time + std::time::Duration::from_secs(125);
        assert_eq!(panel.format_timestamp(ts), "02:05");
    }

    #[test]
    fn test_default_panel_height() {
        let panel = LogPanel::new();
        assert_eq!(panel.panel_height, DEFAULT_HEIGHT);
        assert!(panel.drag_state.is_none());
    }

    #[test]
    fn test_height_constants() {
        assert!(MIN_HEIGHT < DEFAULT_HEIGHT);
        assert!(DEFAULT_HEIGHT < MAX_HEIGHT);
        assert_eq!(MIN_HEIGHT, 38.0);
        assert_eq!(MAX_HEIGHT, 475.0);
        assert_eq!(DEFAULT_HEIGHT, 49.0);
    }

    #[test]
    fn test_default_active_tab_is_log() {
        let panel = LogPanel::new();
        assert_eq!(panel.active_tab, BottomPanelTab::Log);
    }

    #[test]
    fn test_bottom_panel_tab_default_trait() {
        assert_eq!(BottomPanelTab::default(), BottomPanelTab::Log);
    }

    #[test]
    fn test_switch_active_tab() {
        let mut panel = LogPanel::new();
        assert_eq!(panel.active_tab, BottomPanelTab::Log);

        panel.active_tab = BottomPanelTab::Info;
        assert_eq!(panel.active_tab, BottomPanelTab::Info);

        panel.active_tab = BottomPanelTab::Log;
        assert_eq!(panel.active_tab, BottomPanelTab::Log);
    }

    #[test]
    fn test_bottom_panel_tab_equality() {
        assert_eq!(BottomPanelTab::Log, BottomPanelTab::Log);
        assert_eq!(BottomPanelTab::Info, BottomPanelTab::Info);
        assert_ne!(BottomPanelTab::Log, BottomPanelTab::Info);
    }
}
