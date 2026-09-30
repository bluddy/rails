#!/usr/bin/env python3
"""
Generate multi-platform icons (PNG, ICO, ICNS) from data/LOGO.png
"""
from PIL import Image
import os

root_dir = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
logo_path = os.path.join(root_dir, "data", "LOGO.png")
packaging_dir = os.path.join(root_dir, "packaging")

if not os.path.exists(logo_path):
    print(f"Error: {logo_path} not found")
    exit(1)

im = Image.open(logo_path).convert("RGBA")

# Create a 256x256 square image with black background (matching DOS palette background)
size = 256
square = Image.new("RGBA", (size, size), (0, 0, 0, 255))

# Scale logo to fit width (256 x 160)
aspect = im.size[1] / im.size[0]
new_w = size
new_h = int(size * aspect)
resized_logo = im.resize((new_w, new_h), Image.Resampling.LANCZOS)

# Paste centered vertically
y_offset = (size - new_h) // 2
square.paste(resized_logo, (0, y_offset))

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
