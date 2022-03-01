open Containers

(*
PANI format (format flag PANI)

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

let pani_of_stream (s:(int*char) Gen.t) =
  let pani = Gen.take 4 s |> My_gen.to_stringi in
  if String.(pani = "PANI")
  then ()
  else failwith "Not a PANI file";
  let pani_byte1 = My_gen.get_bytei s in
  let pani_byte2 = My_gen.get_bytei s in
  let pani_byte3 = My_gen.get_bytei s in
  Printf.printf "byte3: 0x%x\n" pani_byte3; (* debug *)
  let header_type = My_gen.get_bytei s in
  Printf.printf "header_type: 0x%x\n" header_type; (* debug *)
  let subheader =
    match header_type with
    | 0 -> Gen.take 17 s |> My_gen.to_stringi
    | 1 -> ""
    | 2 -> Gen.take 774 s |>  My_gen.to_stringi
    | n -> failwith @@ Printf.sprintf "Bad header_type %d" n
  in
  ()


  (*
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
  *)


let main filename =
  Printf.printf "--- PANI dump: %s\n" filename;

  (* let filepath = Filename.remove_extension filename in *)

  let str =
    IO.with_in filename @@
      fun in_channel -> IO.read_all in_channel
  in
  let stream = My_gen.of_stringi str in
  pani_of_stream stream


