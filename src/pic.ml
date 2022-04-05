open Containers

(*
PIC format (format flag 0xF)

ofs | datatype | description
----+----------+------------
0 | 16 bits | Format flag.
2 | 16 bits | Width, always 320
4 | 16 bits | Height, always 200
6 | 16 bytes | Pixel color mappings for cga mode (apparently)
22 | byte | Max LZW dictionary bit width, always 0xB
23 | LZW data | image data in LZW+RLE compressed format

PIC format (format flag 0x7)

ofs | datatype | description
----+----------+------------
0 | 16 bits | Format flag.
2 | 16 bits | Width, always 320
4 | 16 bits | Height, always 200
6 | byte | Max LZW dictionary bit width, always 0x0B
7 | LZW data | image data in LZW+RLE compressed format

Each pixel byte represents two 16-color pixels. I'm not sure what they're doing with palette, it's
possible they're just using the "normal" 16 color palette.
*)

let debug = ref false
let debug_images = ref false

module Ndarray = Owl_base_dense_ndarray.Generic

type ndarray = (int, Bigarray.int8_unsigned_elt) Ndarray.t

let ndarray_of_stream (stream: (int * char) Gen.t) : ndarray =
  let format_flag = My_gen.get_wordi stream in
  let bytes_not_word_flag = format_flag land 0x1 in
  let discard_bytes =
    if format_flag land 0x8 = 0x8 then 16
    else if format_flag land 0x10 = 0x10 then 128
    else 0
  in
  let width  = My_gen.get_wordi stream in (* 2 *)
  let height = My_gen.get_wordi stream in (* 4 *)

  if !debug then
    Printf.printf "0x%x: format: 0x%x, width: %d, height: %d, bytes_not_word:%d\n" (My_gen.pos ()) format_flag width height bytes_not_word_flag; (* debug *)

  for _i=0 to discard_bytes - 1 do
    (* print_endline "discard"; *)
    let byte = My_gen.get_bytei stream in
    if !debug then
      Printf.printf "%d " byte;
  done;

  if !debug then print_newline ();

  let lzw_max_byte= My_gen.get_bytei stream in

  if !debug then
    Printf.printf "Max lzw = 0x%x\n" lzw_max_byte;

  (* Printf.printf "offset before lzw: %d\n" (My_gen.pos ()); (* debug *) *)

  let lzw_stream = Lzw.decompress stream ~max_bit_size:11 in
  (* Printf.printf "Length LZW decompressed: %d\n" (Bytes.length bytes); *)

  let lre_stream = Lzw.decode_rle lzw_stream in
  (* Printf.printf "Length rle decompressed: %d\n" (String.length s); *)

  (* We read a line at a time and discard a nibble if it works out that way *)

  let arr = Ndarray.empty Int8_unsigned [|height; width|] in
  for i=0 to height-1 do
    let num_bytes = int_of_float @@ ceil @@ (float_of_int width) /. 2. in
    let str = Gen.take num_bytes lre_stream |> Gen.to_string in
    for j=0 to width-1 do
      let c = Char.code str.[j/2] in
      let nibble = if j land 1 = 0 then c land 0xf else (c lsr 4) land 0xf in
      Ndarray.set arr [|i; j|] nibble;
    done
  done;
  arr

let ndarray_of_file filename : ndarray =
  let str = IO.with_in filename IO.read_all in
  let stream = My_gen.of_stringi str in
  ndarray_of_stream stream

let translate_ega arr ~f ~w ~h =
  for y=0 to h-1 do
    for x=0 to w-1 do
      let write_color x y index =
        let color, alpha = Ega.get_color index ~debug:!debug_images in
        let r, g, b = color lsr 16, (color lsr 8) land 0xFF, color land 0xFF in
        (* Printf.printf "x:%d y:%d\n" x y; *)
        f ~x ~y ~r ~g ~b ~alpha;
      in
      write_color x y @@ Ndarray.get arr [|y;x|]
    done
  done;
  ()

let img_write arr ~x ~y ~r ~g ~b ~alpha =
  Ndarray.set arr [|y;x;0|] r;
  Ndarray.set arr [|y;x;1|] g;
  Ndarray.set arr [|y;x;2|] b;
  Ndarray.set arr [|y;x;3|] alpha;
  ()


let create_rgb_img (w, h) : ndarray =
  Ndarray.empty Int8_unsigned [|h; w; 4|]

let white_pixel =
  let p = create_rgb_img (1, 1) in
  img_write p ~x:0 ~y:0 ~r:255 ~g:255 ~b:255 ~alpha:255;
  p

(*
    ndarray: ndarray with 0-15 palette values
    img: ndarray converted to RGB uint8 values
    png: png file
 *)

let img_of_ndarray (arr:ndarray) =
  let dims = Ndarray.shape arr in
  let w, h = dims.(1), dims.(0) in
  let img = create_rgb_img (w, h) in
  translate_ega arr ~w ~h ~f:(img_write img);
  img

  (* RGBA images *)
let img_of_file in_file =
  let arr = ndarray_of_file in_file in
  img_of_ndarray arr

let png_of_ndarray (arr:ndarray) ~filename =
  let dims = Ndarray.shape arr in
  let w, h = dims.(1), dims.(0) in
  let img = Image.create_rgb w h in
  let f ~x ~y ~r ~g ~b ~alpha =
    let _a = alpha in
    Image.write_rgb img x y r g b in
  translate_ega arr ~f ~w ~h;
  let och = Png.chunk_writer_of_path filename in
  ImagePNG.write och img

let png_of_stream stream ~filename =
  let arr = ndarray_of_stream stream in
  png_of_ndarray arr ~filename

let png_of_file in_file =
  Printf.printf "--- Pic dump: %s\n" in_file;
  let filepath = Filename.remove_extension in_file in
  let destpath = filepath ^ ".png" in
  let arr = ndarray_of_file in_file in
  png_of_ndarray arr ~filename:destpath

