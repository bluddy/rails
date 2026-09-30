#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"

EXE_PATH="${1:-${ROOT_DIR}/_build/default/src/rails_run.exe}"
VERSION="${2:-dev}"
ARCH="${3:-arm64}"
OUTPUT_ZIP="${4:-rails-${VERSION}-macos-${ARCH}.zip}"

STAGE_DIR="${ROOT_DIR}/stage-macos"
rm -rf "$STAGE_DIR"
APP_DIR="$STAGE_DIR/Rails.app"
CONTENTS="$APP_DIR/Contents"
MACOS_DIR="$CONTENTS/MacOS"
RESOURCES_DIR="$CONTENTS/Resources"
FRAMEWORKS_DIR="$CONTENTS/Frameworks"

mkdir -p "$MACOS_DIR" "$RESOURCES_DIR/data" "$FRAMEWORKS_DIR"

echo "==> Preparing macOS App Bundle..."
cp "$EXE_PATH" "$MACOS_DIR/rails"
chmod +x "$MACOS_DIR/rails"

cp -r "${ROOT_DIR}/shaders" "$RESOURCES_DIR/"
cp -r "${ROOT_DIR}/sound" "$RESOURCES_DIR/"
cp -r "${ROOT_DIR}/music" "$RESOURCES_DIR/"

if [ -f "${ROOT_DIR}/data/FONTS.RR" ]; then
    cp "${ROOT_DIR}/data/FONTS.RR" "$RESOURCES_DIR/data/"
fi
if [ -f "${ROOT_DIR}/data/SPRITES_extra.png" ]; then
    cp "${ROOT_DIR}/data/SPRITES_extra.png" "$RESOURCES_DIR/data/"
fi
if [ -f "${ROOT_DIR}/data/TRACKS_extra.png" ]; then
    cp "${ROOT_DIR}/data/TRACKS_extra.png" "$RESOURCES_DIR/data/"
fi

cp "${ROOT_DIR}/packaging/PUT_ORIGINAL_GAME_FILES_HERE.txt" "$RESOURCES_DIR/data/"
cp "${ROOT_DIR}/packaging/README_RELEASE.txt" "$STAGE_DIR/README.txt"

# Info.plist
cat << EOF > "$CONTENTS/Info.plist"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>CFBundleDevelopmentRegion</key>
    <string>en</string>
    <key>CFBundleExecutable</key>
    <string>rails</string>
    <key>CFBundleIdentifier</key>
    <string>org.bluddy.rails</string>
    <key>CFBundleInfoDictionaryVersion</key>
    <string>6.0</string>
    <key>CFBundleName</key>
    <string>Railroad Tycoon</string>
    <key>CFBundlePackageType</key>
    <string>APPL</string>
    <key>CFBundleShortVersionString</key>
    <string>${VERSION}</string>
    <key>CFBundleVersion</key>
    <string>${VERSION}</string>
    <key>CFBundleIconFile</key>
    <string>rails.icns</string>
    <key>LSMinimumSystemVersion</key>
    <string>11.0</string>
    <key>NSHighResolutionCapable</key>
    <true/>
</dict>
</plist>
EOF

if [ -f "${ROOT_DIR}/packaging/rails.icns" ]; then
    cp "${ROOT_DIR}/packaging/rails.icns" "$RESOURCES_DIR/rails.icns"
fi

# Copy and fix dylibs if running on macOS with dylibbundler or Homebrew
if command -v dylibbundler >/dev/null 2>&1; then
    echo "==> Bundling dynamic libraries using dylibbundler..."
    dylibbundler -od -b -x "$MACOS_DIR/rails" -d "$FRAMEWORKS_DIR" -p "@executable_path/../Frameworks"
elif command -v otool >/dev/null 2>&1; then
    echo "==> Collecting Homebrew dylibs manually..."
    # Find SDL2 and SDL2_mixer in Homebrew prefix
    BREW_PREFIX="$(brew --prefix 2>/dev/null || echo "/opt/homebrew")"
    for dylib in $(otool -L "$MACOS_DIR/rails" | awk '{print $1}' | grep -E '(SDL2|SDL2_mixer)'); do
        if [ -f "$dylib" ]; then
            cp "$dylib" "$FRAMEWORKS_DIR/"
            base=$(basename "$dylib")
            install_name_tool -change "$dylib" "@executable_path/../Frameworks/$base" "$MACOS_DIR/rails"
        fi
    done
fi

echo "==> Creating macOS release archive: $OUTPUT_ZIP..."
(cd "$STAGE_DIR" && zip -r -q "${ROOT_DIR}/${OUTPUT_ZIP}" .)
rm -rf "$STAGE_DIR"
echo "==> macOS release created: ${OUTPUT_ZIP}"
