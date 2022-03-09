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

let str_of_stream (stream: (int * char) Gen.t) =
  let discard_bytes =
    let rec loop () =
      match My_gen.get_wordi stream with
      | 0x0 -> loop ()
      | 0xF -> 23 - 6
      | 0x7 | 0x6 -> 7 - 6 
      | x -> failwith @@ Printf.sprintf "Unknown format 0x%x" x
    in
    loop ()
  in
  let width  = My_gen.get_wordi stream in (* 2 *)
  let height = My_gen.get_wordi stream in (* 4 *)

  if !debug then
    Printf.printf "0x%x: width: %d, height: %d\n" (My_gen.pos ()) width height; (* debug *)

  for _i=0 to discard_bytes - 1 do
    My_gen.junki stream
  done;

  (* Printf.printf "offset before lzw: %d\n" (My_gen.pos ()); (* debug *) *)

  let lzw_stream = Lzw.decompress stream ~max_bit_size:11 in
  (* Printf.printf "Length LZW decompressed: %d\n" (Bytes.length bytes); *)

  let lre_stream = Lzw.decode_rle lzw_stream in
  (* Printf.printf "Length rle decompressed: %d\n" (String.length s); *)

  (* We read a line at a time and discard a nibble if it works out that way *)
  let buffer = Buffer.create 10 in
  for i=0 to height-1 do
    let num_bytes = int_of_float @@ ceil @@ (float_of_int width) /. 2. in
    let str = Gen.take num_bytes lre_stream |> Gen.to_string in
    Buffer.add_string buffer str;
  done;
  Buffer.contents buffer, width, height

let load_to_str filename =
  let str =
    IO.with_in filename @@ fun in_channel -> IO.read_all in_channel
  in
  let stream = My_gen.of_stringi str in
  str_of_stream stream

module Ndarray = Owl_base_dense_ndarray.Generic

let bigarray_of_str str ~w ~h =
  let exception Done in
  (* Convert str to bigarray of nibbles *)
  let arr = Ndarray.empty Int8_unsigned [|h; w|] in
  let idx = ref 0 in
  let low = ref true in (* low then high *)
  begin try
    for y=0 to h-1 do
      for x=0 to w-1 do
        let c = Char.code str.[!idx] in
        let nibble = if !low then c land 0xf else (c lsr 4) land 0xf in
        Ndarray.set arr [|y;x|] nibble;
        (* advance *)
        match !low with
        | true  ->
            low := false
        | false ->
            begin
              low := true;
              incr idx;
              if !idx >= String.length str then raise_notrace Done
            end
      done
    done
  with
  | Done -> () end;
  arr

let translate_ega arr ~f ~w ~h =
  for y=0 to h-1 do
    for x=0 to w-1 do
      let write_color x y index =
        let color = Ega.get_color index in
        let r, g, b = color lsr 16, (color lsr 8) land 0xFF, color land 0xFF in
        (* Printf.printf "x:%d y:%d\n" x y; *)
        f x y r g b;
      in
      write_color x y @@ Ndarray.get arr [|y;x|]
    done
  done;
  ()

let img_write ?(a=0xFF) arr x y (r:int) (g:int) (b:int) =
  Ndarray.set arr [|y;x;0|] r;
  Ndarray.set arr [|y;x;1|] g;
  Ndarray.set arr [|y;x;2|] b;
  Ndarray.set arr [|y;x;3|] a;
  ()

let create_rgb_img ~w ~h =
  Ndarray.empty Int8_unsigned [|h; w; 4|]

let bigarray_of_file filename =
  let str, w, h = load_to_str filename in
  bigarray_of_str str ~w ~h

let img_of_bigarray arr =
  let dims = Ndarray.shape arr in
  let w, h = dims.(1), dims.(0) in
  let img = create_rgb_img ~w ~h in
  translate_ega arr ~w ~h ~f:(img_write img);
  img

let ndarray_of_stream stream =
  let str, w, h = str_of_stream stream in
  bigarray_of_str str ~w ~h

let img_of_file filename =
  let str, w, h = load_to_str filename in
  let arr = bigarray_of_str str ~w ~h in
  let img = create_rgb_img ~w ~h in
  translate_ega arr ~w ~h ~f:(img_write img);
  img

let png_of_str str w h ~filename =
  let ndarray = bigarray_of_str str ~w ~h in
  let img = Image.create_rgb w h in
  translate_ega ndarray ~f:(Image.write_rgb img) ~w ~h;
  let och = Png.chunk_writer_of_path filename in
  ImagePNG.write och img

let png_of_stream stream ~filename =
  let str, w, h = str_of_stream stream in
  png_of_str str w h ~filename

let png_of_file filename =
  Printf.printf "--- Pic dump: %s\n" filename;
  let filepath = Filename.remove_extension filename in
  let destpath = filepath ^ ".png" in
  let str, w, h = load_to_str filename in
  png_of_str str w h ~filename:destpath

