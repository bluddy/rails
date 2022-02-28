open Containers
open Iter.Infix

(*
PANI format (format flag PANI)

ofs  | datatype | description
-----+----------+------------
0x0  | PANI     |
0x4  | byte     | Always 3
0x5  | byte
0x6  | file_type | 0: 17 byte header. 1: no header. 2: 774 byte header

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

IRONM: 42sec
WOOD2: 18sec
*)

(* Fill a color Image based on image bits *)
let fill_image img_str img width height =
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
  done

let main filename =
  Printf.printf "--- PANI dump: %s\n" filename;

  let filepath = Filename.remove_extension filename in

  let buffer =
    IO.with_in filename @@
      fun in_channel -> IO.read_all in_channel
  in
  let buf_stream = String.to_seqi buffer in
  
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
    fill_image img_str img width height;

    (* Dump PNG *)
    let och = Png.chunk_writer_of_path destpath in
    ImagePNG.write och img
  )
  offset_list




