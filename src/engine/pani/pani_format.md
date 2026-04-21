# PANI format (format flag PANI)

ofs  | datatype | description
-----+----------+------------
0x0  | PANI     |
0x4  | pani_byte1     | Always 3
0x5  | pani_byte2
0x6  | pani_byte3 | 0: skip next part 1: don't skip
0x6  | header_type | 0: 17 byte header. 1: no header. 2: 774 byte header

No header:
0x7  | 9 words  | PANI struct
0x18 | byte     | If 0, nothing. If 2, set ega params. If 1, image

read 250 words, look for non-zero
word: number of 16 byte blocks

0x24 | 16 bits  | Format flag.
0x26 | 16 bits  | Width, always 320
0x28 | 16 bits  | Height, always 200
0x2A | byte     | Max LZW dictionary bit width, always 0x0B
0x2B | LZW data | image data in LZW+RLE compressed format

## Layout
First image
Gap of 500 bytes (with some data)
   - 0s except for a short for each image!
   - Increasing numbers
then next images

## Data collected
~1041: 14 images
WRECKM: 3664 105 images, 148 times 0500
FOLLOWED.PAN 1680: 29 images, 42 times 0500
FLOODM.PAN 88 images, 172 times 0500
WOOD2 52 images
HQ 14 images: 0-E, E1c start: 0, 1, 2, 5, 6, 7, 8, b, c, d, e, f, 10, 11
CAPTURED: 0:bckgrnd, 1:bcgrnd2 5,6,7,8,9: over same spot (captured)
- possibly mapped to 2, 4, 6, 8, 9, C, E, 10, 12

## Program section
* ends with 0a

CAPTRED
- Commands separated by 0500
- 2300: animation, long

IRONM: 42sec
WOOD2: 18sec
*)
