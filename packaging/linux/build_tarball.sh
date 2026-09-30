#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(cd "${SCRIPT_DIR}/../.." && pwd)"

EXE_PATH="${1:-${ROOT_DIR}/_build/default/src/rails_run.exe}"
VERSION="${2:-dev}"
OUTPUT_TAR="${3:-rails-${VERSION}-linux-x86_64.tar.gz}"

STAGE_DIR="${ROOT_DIR}/stage-linux"
rm -rf "$STAGE_DIR"
mkdir -p "$STAGE_DIR/rails/data"

echo "==> Preparing Linux portable package..."
cp "$EXE_PATH" "$STAGE_DIR/rails/rails"
chmod +x "$STAGE_DIR/rails/rails"

cp -r "${ROOT_DIR}/shaders" "$STAGE_DIR/rails/"
cp -r "${ROOT_DIR}/sound" "$STAGE_DIR/rails/"
cp -r "${ROOT_DIR}/music" "$STAGE_DIR/rails/"

if [ -f "${ROOT_DIR}/data/FONTS.RR" ]; then
    cp "${ROOT_DIR}/data/FONTS.RR" "$STAGE_DIR/rails/data/"
fi
if [ -f "${ROOT_DIR}/data/SPRITES_extra.png" ]; then
    cp "${ROOT_DIR}/data/SPRITES_extra.png" "$STAGE_DIR/rails/data/"
fi
if [ -f "${ROOT_DIR}/data/TRACKS_extra.png" ]; then
    cp "${ROOT_DIR}/data/TRACKS_extra.png" "$STAGE_DIR/rails/data/"
fi

cp "${ROOT_DIR}/packaging/PUT_ORIGINAL_GAME_FILES_HERE.txt" "$STAGE_DIR/rails/data/"
cp "${ROOT_DIR}/packaging/README_RELEASE.txt" "$STAGE_DIR/rails/README.txt"

tar -czf "$OUTPUT_TAR" -C "$STAGE_DIR" rails
rm -rf "$STAGE_DIR"
echo "==> Created portable tarball: $OUTPUT_TAR"
