//! Data Inspector functionality for the hex editor
//!
//! This module contains methods for the Data Inspector panel:
//! - Toggle visibility
//! - Toggle endianness

use crate::HexEditor;
use crate::ui::Endian;

impl HexEditor {
    /// Toggle data inspector visibility
    pub fn toggle_inspector(&mut self) {
        self.inspector_visible = !self.inspector_visible;
    }

    /// Toggle inspector endianness
    pub fn toggle_inspector_endian(&mut self) {
        self.inspector_endian = match self.inspector_endian {
            Endian::Little => Endian::Big,
            Endian::Big => Endian::Little,
        };
    }
}
