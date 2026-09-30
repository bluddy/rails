#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"

EXE_PATH="${1:-${ROOT_DIR}/_build/default/src/rails_run.exe}"
OUTPUT_NAME="${2:-rails-linux-x86_64.AppImage}"

if [ ! -f "$EXE_PATH" ]; then
    echo "Executable not found at: $EXE_PATH"
    echo "Please build with 'dune build' first."
    exit 1
fi

APP_DIR="${ROOT_DIR}/AppDir"
rm -rf "$APP_DIR"
mkdir -p "$APP_DIR/usr/bin" "$APP_DIR/usr/lib" "$APP_DIR/usr/share/rails/data"

echo "==> Copying binary..."
cp "$EXE_PATH" "$APP_DIR/usr/bin/rails"
chmod +x "$APP_DIR/usr/bin/rails"

echo "==> Copying assets..."
cp -r "${ROOT_DIR}/shaders" "$APP_DIR/usr/share/rails/"
cp -r "${ROOT_DIR}/sound" "$APP_DIR/usr/share/rails/"
cp -r "${ROOT_DIR}/music" "$APP_DIR/usr/share/rails/"

# Copy bundled data assets
if [ -f "${ROOT_DIR}/data/FONTS.RR" ]; then
    cp "${ROOT_DIR}/data/FONTS.RR" "$APP_DIR/usr/share/rails/data/"
fi
if [ -f "${ROOT_DIR}/data/SPRITES_extra.png" ]; then
    cp "${ROOT_DIR}/data/SPRITES_extra.png" "$APP_DIR/usr/share/rails/data/"
fi
if [ -f "${ROOT_DIR}/data/TRACKS_extra.png" ]; then
    cp "${ROOT_DIR}/data/TRACKS_extra.png" "$APP_DIR/usr/share/rails/data/"
fi

echo "==> Copying desktop file and icons..."
cp "${ROOT_DIR}/packaging/rails.desktop" "$APP_DIR/rails.desktop"
cp "${ROOT_DIR}/packaging/rails.png" "$APP_DIR/rails.png"
cp "${ROOT_DIR}/packaging/rails.png" "$APP_DIR/.DirIcon"

echo "==> Creating AppRun..."
cat << 'EOF' > "$APP_DIR/AppRun"
#!/bin/sh
SELF=$(readlink -f "$0")
HERE=${SELF%/*}
export APPDIR="$HERE"
export PATH="$HERE/usr/bin:$PATH"
export LD_LIBRARY_PATH="$HERE/usr/lib:$LD_LIBRARY_PATH"
export RAILS_ASSETS_DIR="$HERE/usr/share/rails"

# Locate data directory
if [ -z "$RAILS_DATA_DIR" ]; then
    if [ -n "$APPIMAGE" ]; then
        APPIMAGE_DIR=$(dirname "$APPIMAGE")
        if [ -d "$APPIMAGE_DIR/data" ]; then
            export RAILS_DATA_DIR="$APPIMAGE_DIR/data"
        fi
    fi
fi

if [ -z "$RAILS_DATA_DIR" ] && [ -n "$OWD" ] && [ -d "$OWD/data" ]; then
    export RAILS_DATA_DIR="$OWD/data"
fi

if [ -z "$RAILS_DATA_DIR" ] && [ -d "$PWD/data" ]; then
    export RAILS_DATA_DIR="$PWD/data"
fi

exec "$HERE/usr/bin/rails" "$@"
EOF
chmod +x "$APP_DIR/AppRun"

echo "==> Copying shared libraries (SDL2, SDL2_mixer, codecs)..."
# Find and bundle shared libraries needed by SDL2 and SDL2_mixer
copy_lib() {
    local lib="$1"
    local found
    found=$(ldconfig -p | grep " => " | grep -E "\b${lib}\b" | head -n1 | awk '{print $4}' || true)
    if [ -n "$found" ] && [ -f "$found" ]; then
        cp -L "$found" "$APP_DIR/usr/lib/" 2>/dev/null || true
    fi
}

copy_lib "libSDL2-2.0.so.0"
copy_lib "libSDL2_mixer-2.0.so.0"
copy_lib "libvorbisfile.so.3"
copy_lib "libvorbis.so.0"
copy_lib "libogg.so.0"
copy_lib "libFLAC.so.12"
copy_lib "libmpg123.so.0"
copy_lib "libmodplug.so.1"
copy_lib "libopusfile.so.0"
copy_lib "libopus.so.0"
copy_lib "libfluidsynth.so.3"

echo "AppDir prepared successfully at: $APP_DIR"

TOOL="${APPIMAGETOOL:-appimagetool}"
if command -v "$TOOL" >/dev/null 2>&1 || [ -x "$TOOL" ]; then
    echo "==> Building AppImage with $TOOL..."
    ARCH=x86_64 "$TOOL" --appimage-extract-and-run "$APP_DIR" "$OUTPUT_NAME" 2>/dev/null || ARCH=x86_64 "$TOOL" "$APP_DIR" "$OUTPUT_NAME"
    echo "==> AppImage created: $OUTPUT_NAME"
else
    echo "appimagetool not found; AppDir is ready at: $APP_DIR"
fi
