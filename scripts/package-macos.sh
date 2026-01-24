#!/bin/bash
#
# macOS App Bundle Packaging Script for RustHex
# Target: aarch64-apple-darwin (Apple Silicon)
#

set -e

# Configuration
APP_NAME="RustHex"
BUNDLE_ID="com.github.suma.rusthex"
VERSION=$(grep '^version' Cargo.toml | head -1 | sed 's/.*"\(.*\)".*/\1/')
TARGET="aarch64-apple-darwin"
BUILD_DIR="target/${TARGET}/release"
DIST_DIR="dist"
APP_BUNDLE="${DIST_DIR}/${APP_NAME}.app"

echo "=== RustHex macOS Packaging Script ==="
echo "Version: ${VERSION}"
echo "Target: ${TARGET}"
echo ""

# Check if we're in the project root
if [ ! -f "Cargo.toml" ]; then
    echo "Error: Please run this script from the project root directory"
    exit 1
fi

# Build release binary
echo "Building release binary..."
cargo build --release --target ${TARGET}

if [ ! -f "${BUILD_DIR}/rusthex" ]; then
    echo "Error: Build failed - binary not found"
    exit 1
fi

# Create distribution directory
echo "Creating app bundle..."
rm -rf "${APP_BUNDLE}"
mkdir -p "${APP_BUNDLE}/Contents/MacOS"
mkdir -p "${APP_BUNDLE}/Contents/Resources"

# Copy binary
cp "${BUILD_DIR}/rusthex" "${APP_BUNDLE}/Contents/MacOS/${APP_NAME}"
chmod +x "${APP_BUNDLE}/Contents/MacOS/${APP_NAME}"

# Create Info.plist
cat > "${APP_BUNDLE}/Contents/Info.plist" << EOF
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleName</key>
    <string>${APP_NAME}</string>
    <key>CFBundleDisplayName</key>
    <string>${APP_NAME}</string>
    <key>CFBundleIdentifier</key>
    <string>${BUNDLE_ID}</string>
    <key>CFBundleVersion</key>
    <string>${VERSION}</string>
    <key>CFBundleShortVersionString</key>
    <string>${VERSION}</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleExecutable</key>
    <string>${APP_NAME}</string>
    <key>CFBundleIconFile</key>
    <string>AppIcon</string>
    <key>LSMinimumSystemVersion</key>
    <string>11.0</string>
    <key>NSHighResolutionCapable</key>
    <true/>
    <key>NSSupportsAutomaticGraphicsSwitching</key>
    <true/>
    <key>CFBundleDocumentTypes</key>
    <array>
        <dict>
            <key>CFBundleTypeName</key>
            <string>All Files</string>
            <key>CFBundleTypeRole</key>
            <string>Editor</string>
            <key>LSHandlerRank</key>
            <string>Alternate</string>
            <key>LSItemContentTypes</key>
            <array>
                <string>public.data</string>
                <string>public.item</string>
            </array>
        </dict>
    </array>
    <key>NSAppleEventsUsageDescription</key>
    <string>RustHex needs access to handle file operations.</string>
</dict>
</plist>
EOF

# Create PkgInfo
echo -n "APPL????" > "${APP_BUNDLE}/Contents/PkgInfo"

# Check if icon exists, if not create a placeholder message
if [ -f "assets/AppIcon.icns" ]; then
    cp "assets/AppIcon.icns" "${APP_BUNDLE}/Contents/Resources/"
    echo "Icon: Copied AppIcon.icns"
else
    echo "Note: No icon found at assets/AppIcon.icns - app will use default icon"
fi

echo ""
echo "=== Build Complete ==="
echo "App bundle: ${APP_BUNDLE}"
echo ""

# Get binary size
BINARY_SIZE=$(du -h "${APP_BUNDLE}/Contents/MacOS/${APP_NAME}" | cut -f1)
echo "Binary size: ${BINARY_SIZE}"

# Optionally create DMG
if command -v hdiutil &> /dev/null; then
    echo ""
    read -p "Create DMG file? (y/N): " CREATE_DMG
    if [ "$CREATE_DMG" = "y" ] || [ "$CREATE_DMG" = "Y" ]; then
        DMG_NAME="${APP_NAME}-${VERSION}-aarch64.dmg"
        DMG_PATH="${DIST_DIR}/${DMG_NAME}"

        echo "Creating DMG..."
        rm -f "${DMG_PATH}"

        # Create temporary DMG directory
        DMG_TEMP="${DIST_DIR}/dmg-temp"
        rm -rf "${DMG_TEMP}"
        mkdir -p "${DMG_TEMP}"
        cp -R "${APP_BUNDLE}" "${DMG_TEMP}/"

        # Create symbolic link to Applications folder
        ln -s /Applications "${DMG_TEMP}/Applications"

        # Create DMG
        hdiutil create -volname "${APP_NAME}" \
            -srcfolder "${DMG_TEMP}" \
            -ov -format UDZO \
            "${DMG_PATH}"

        rm -rf "${DMG_TEMP}"

        DMG_SIZE=$(du -h "${DMG_PATH}" | cut -f1)
        echo ""
        echo "DMG created: ${DMG_PATH}"
        echo "DMG size: ${DMG_SIZE}"
    fi
fi

echo ""
echo "=== Done ==="
echo ""
echo "To run the app:"
echo "  open ${APP_BUNDLE}"
echo ""
echo "To install:"
echo "  cp -R ${APP_BUNDLE} /Applications/"
