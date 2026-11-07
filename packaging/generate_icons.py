#!/usr/bin/env python3
"""
Generate multi-platform icons (PNG, ICO, ICNS) from packaging/icon_src.png
"""
from PIL import Image
import os

root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
packaging_dir = os.path.join(root_dir, "packaging")
logo_path = os.path.join(packaging_dir, "icon_src.png")

if not os.path.exists(logo_path):
    print(f"Error: {logo_path} not found")
    exit(1)

im = Image.open(logo_path).convert("RGBA")

# Crop a square anchored at the bottom (removing excess top area) and centered horizontally
crop_size = 232
x0 = 46  # centered on logo + both MICRO PROSE boxcars (x=46..278)
y0 = im.height - crop_size  # bottom-anchored (y=9..241)
cropped = im.crop((x0, y0, x0 + crop_size, y0 + crop_size))

size = 256
square = cropped.resize((size, size), Image.Resampling.NEAREST)

# Save 256x256 PNG
png_path = os.path.join(packaging_dir, "rails.png")
square.save(png_path, format="PNG")
print(f"Saved: {png_path}")

# Save Windows multi-size ICO
ico_path = os.path.join(packaging_dir, "rails.ico")
icon_sizes = [(16, 16), (32, 32), (48, 48), (64, 64), (128, 128), (256, 256)]
square.save(ico_path, format="ICO", sizes=icon_sizes)
print(f"Saved: {ico_path}")

# Save macOS ICNS if supported by Pillow
icns_path = os.path.join(packaging_dir, "rails.icns")
try:
    square.save(icns_path, format="ICNS")
    print(f"Saved: {icns_path}")
except Exception as e:
    print(f"Could not save ICNS directly ({e}), PNG is available for macOS bundle.")
