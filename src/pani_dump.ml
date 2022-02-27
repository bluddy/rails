open Containers
open Iter.Infix

(*
PANI format (format flag PANI)

ofs | datatype | description
----+----------+------------
0 | PANI
0x24 | 16 bits | Format flag.
0x26 | 16 bits | Width, always 320
0x28 | 16 bits | Height, always 200
0x2A | byte | Max LZW dictionary bit width, always 0x0B
0x2B | LZW data | image data in LZW+RLE compressed format

Layout
------
First image
Gap of 500 bytes (with some data)
   - 0s except for a short for each image!
   - Increasing numbers
then next images

Data collected
--------------
~1041: 14 images
WRECKM: 3664 105 images, 148 times 0500
FOLLOWED.PAN 1680: 29 images, 42 times 0500
FLOODM.PAN 88 images, 172 times 0500
WOOD2 52 images
HQ 14 images: 0-E, E1c start: 0, 1, 2, 5, 6, 7, 8, b, c, d, e, f, 10, 11
CAPTURED: 0:bckgrnd, 1:bcgrnd2 5,6,7,8,9: over same spot (captured)
- possibly mapped to 2, 4, 6, 8, 9, C, E, 10, 12

Program section
---------------
* ends with 0a

CAPTRED
- Commands separated by 0500
- 2300: animation, long

          0500 ffff 0600 00 0500 ea 00 05
# ffff -> maybe z layer?
00 13 00 0500 ffff 0500 0000 0500 0000 05
# 13 is the command?
# ffff, 0000, 0000: move line to right, down
00 ff 0005 0001 0000 0500 ed00 0500 1400 # ed is critical
0500 ffff 0500 5d00 0500 0000 0500 ff00
0500 0000 0005 00e7 0005 0003 0005 00ff
ff05 0000 0005 0030 0005 00ff 0005 0000
0000 0500 f600 0500 0700 0500 ffff 0500
# ffff->0000: raise whole animation of cpatured!
0100 0500 7100 0500 ff00 0500 0000 0005
# 0100->2000: move whole animation to right
# 7100->0000: move whole animation way up!
# 0000 00 -> disturb these values and the animation goes wonky
0100 0013 8500 13dd 00 07 05 00 0100 0812 (* 07 -> 06, or 05->06 man to lady! *)
9100 13b1 0005 00f3 0005 0004 0005 00ff
ff05 0012 0005 0031 0005 00ff 0005 0000
0000 1382 0007 0500 ffff 0812 bd00 13dd
0005 00f0 0005 0004 0005 00ff ff05 0012
0005 0031 0005 00ff 0005 0000 0000 1382
0006 ffff 0500 2300 02 14 1400 0209 0008 # 08-> 09: clone the suit?
# 1st 14: type of animation that waits for key. 15: doesn't wait when done (needs 02 first). 16:looping
# 2nd 14: seems irrelevant
0900 0909 0004 0900 0609 050f 0000 ff06 # 0004->02: clone the frame?
f900 00 1300 1202 fcff 0000 0010 0204 00
# fcff 0000: Middle frame right, down (affects last frame too)
# f900 -> f000: duplicate suit where animatino should have been
0000 00 0e02 0200 0200 000c 0900 0005 00
# 0200 0200: final frame right, down (can be neg for left, up)
0000 0500

IRONM: 42sec
WOOD2: 18sec

The image data is stored as LZW compressed RLE stream. The LZW resets when the dictionary gets full (i.e, there's no separate reset signal).
Under the LZW the data is compressed with RLE, so that if a pixel byte is 0x90, the previous pixel is repeated as many times as the next byte says; if the repeat value is 0, the pixel value is 0x90.

To reiterate, the RLE works this way:
aa 90 bb
if bb = 0, output is "aa 90"
if bb != 0, output is "aa" * (bb+1)

And yes, if you want a stream of 90's, you do 90 00 90 xx.

Each pixel byte represents two 16-color pixels. I'm not sure what they're doing with palette, it's
possible they're just using the "normal" 16 color palette.
*)


(*
let main filename =
  Printf.printf "--- PANI dump: %s\n" filename;

  let filepath = Filename.remove_extension filename in

  let bytes =
    IO.with_in filename @@
      fun in_channel -> IO.read_all_bytes in_channel
  in
  (* Find all images *)
  let offset_list =
    let diff = 6 in
    Iter.fold (fun acc i ->
      let v1 = Bytes.get_uint16_le bytes i in
      let b2 = Bytes.get_uint8 bytes (i+diff) in
      match v1, b2 with
      | 0x07, 0xb -> i::acc
      | _ -> acc
    )
    []
    (0x24 -- (Bytes.length bytes - diff - 1))
    |> List.rev
  in
  Printf.printf "Found %d images\n" @@ List.length offset_list;

  List.iteri (fun i offset ->

    let destpath = Printf.sprintf "./png/%s_%03d.png" filepath i in

    let width  = Bytes.get_uint16_le bytes @@ offset + 2 in
    let height = Bytes.get_uint16_le bytes @@ offset + 4 in
    Printf.printf "idx=%d offset=%x width=%d, height=%d, total_size=%d\n" i offset width height (width*height/2);
    Printf.printf "Length original: %d\n" (Bytes.length bytes);

    let start_offset = offset + 7 in
    let bytes = Bytes.sub bytes start_offset (Bytes.length bytes - start_offset) in

    let bytes = Lzw.decompress bytes ~max_bit_size:11 ~report_offset:offset ~suppress_error:true in
    Printf.printf "Length LZW decompressed: %d\n" (Bytes.length bytes);

    let img_str = decode_rle bytes |> Bytes.to_string in
    Printf.printf "Length rle decompressed: %d\n" (String.length img_str);

    let img = Image.create_rgb width height in
    let ega_palette =
      [|0x0; 0xAA; 0xAA00; 0xAAAA; 0xAA0000; 0x000000; 0xAA5500; 0xAAAAAA;
      0x555555; 0x5555FF; 0x55FF55; 0x55ffff; 0xff5555; 0xff55ff; 0xffff55; 0xffffff|]
    in
    let idx = ref 0 in
    let low = ref true in (* low then high *)
    for y=0 to height-1 do
      for x=0 to width-1 do
        let c = int_of_char img_str.[!idx] in
        let nibble = if !low then c land 0x0f else c lsr 4 in
        let write_color x y index =
          let color = ega_palette.(index) in
          let r, g, b = color lsr 16, (color lsr 8) land 0xFF, color land 0xFF in
          (* Printf.printf "x:%d y:%d\n" x y; *)
          Image.write_rgb img x y r g b;
        in
        write_color x y nibble;

        (* advance *)
        if !low && x < width-1 then begin
          low := false
        end else begin
          low := true;
          incr idx
        end
      done
    done;
    let och = Png.chunk_writer_of_path destpath in
    ImagePNG.write och img
  )
  offset_list

*)


