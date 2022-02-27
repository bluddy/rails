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

let load_to_str filename =
  let bytes =
    IO.with_in filename @@
      fun in_channel -> IO.read_all_bytes in_channel
  in
  let start_offset =
    match Bytes.get_uint16_le bytes 0 with
    | 0xF -> 23
    | 0x7 -> 7
    | 0x6 -> 7
    | _ -> failwith "Unknown format"
  in
  let width  = Bytes.get_uint16_le bytes 2 in
  let height = Bytes.get_uint16_le bytes 4 in

  (* Printf.printf "Length original: %d\n" (Bytes.length bytes); *)
  let bytes = Bytes.sub bytes start_offset (Bytes.length bytes - start_offset) in
  let stream = Gen.of_string @@ String.of_bytes bytes in

  let lzw_stream = Lzw.decompress stream ~max_bit_size:11 in
  (* Printf.printf "Length LZW decompressed: %d\n" (Bytes.length bytes); *)

  let lre_stream = Lzw.decode_rle lzw_stream in
  (* Printf.printf "Length rle decompressed: %d\n" (String.length s); *)

  let str = Gen.take (width * height) lre_stream |> Gen.to_string in

  str, width, height

module Ndarray = Owl_base_dense_ndarray.Generic

exception Done

let bigarray_of_str str ~w ~h =
  let arr = Ndarray.empty Int8_unsigned [|h; w|] in
  let idx = ref 0 in
  let low = ref true in (* low then high *)
  begin try
    for y=0 to h-1 do
      for x=0 to w-1 do
        let c = int_of_char str.[!idx] in
        let nibble = if !low then c land 0x0f else c lsr 4 in
        Ndarray.set arr [|y;x|] nibble;
        (* advance *)
        if !low && x < w-1 then begin
          low := false
        end else begin
          low := true;
          incr idx;
          if !idx >= String.length str then raise Done
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

let img_of_file filename =
  let str, w, h = load_to_str filename in
  let arr = bigarray_of_str str ~w ~h in
  let img = create_rgb_img ~w ~h in
  translate_ega arr ~w ~h ~f:(img_write img);
  img

let png_of_file filename =
  Printf.printf "--- Pic dump: %s" filename;
  let filepath = Filename.remove_extension filename in
  let destpath = filepath ^ ".png" in
  let str, w, h = load_to_str filename in
  let arr = bigarray_of_str str ~w ~h in
  let img = Image.create_rgb 320 200 in
  translate_ega arr ~f:(Image.write_rgb img) ~w ~h;
  let och = Png.chunk_writer_of_path destpath in
  ImagePNG.write och img

