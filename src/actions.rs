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
        OpenSelectionInNewTab,
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
        ToggleLogPanel,
        ClearLog,
        // Application
        About,
        // Navigation
        NavigateBack,
        NavigateForward,
        // Search
        FindNext,
        FindPrev,
        // Analyze
        AnalyzeSelection,
    ]
);
