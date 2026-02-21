//! Action definitions for menu bar and keyboard shortcut integration

use gpui::actions;

actions!(
    rusthex,
    [
        // File
        Open,
        Save,
        SaveAs,
        SaveSelectionAs,
        NewTab,
        CloseTab,
        Quit,
        // Edit
        Undo,
        Redo,
        SelectAll,
        Copy,
        Paste,
        ToggleInsertMode,
        // Copy variants
        CopyAsAscii,
        CopyAsHexString,
        CopyAsCArray,
        // View
        ToggleSearch,
        ToggleInspector,
        ToggleBitmap,
        TogglePatternPanel,
        ToggleCompareMode,
        CycleEncoding,
        // Navigation
        NavigateBack,
        NavigateForward,
        // Search
        FindNext,
        FindPrev,
    ]
);
