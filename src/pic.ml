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

let decode_rle bytes =
  let out = Buffer.create 100 in

  let rle = ref false in
  let last_val = ref 0 in
  for i = 0 to Bytes.length bytes - 1 do
    let v = Bytes.get_uint8 bytes i in
    match v with
    | 0x90 ->
        rle := true
    | 0 when !rle ->
        Buffer.add_uint8 out 0x90;
        rle := false;
        last_val := 0x90
    | x when !rle ->
        (* do one less than normal *)
        for _i = 0 to x - 2 do
          Buffer.add_uint8 out !last_val
        done;
        rle := false
    | x ->
        Buffer.add_uint8 out x;
        last_val := x;
  done;
  Buffer.to_bytes out

let int_list_of_bytes bytes =
  let compressed = ref [] in
  for i=0 to Bytes.length bytes - 1 do
    let v = Bytes.get_uint8 bytes i in
    compressed := v :: !compressed;
  done;
  List.rev !compressed

let main filename =
  Printf.printf "--- Pic dump: %s" filename;

  let filepath = Filename.remove_extension filename in
  let destpath = filepath ^ ".png" in

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
  Printf.printf "Length original: %d\n" (Bytes.length bytes);
  let bytes = Bytes.sub bytes start_offset (Bytes.length bytes - start_offset) in

  let bytes = Lzw.decompress bytes ~max_bit_size:11 in
  Printf.printf "Length LZW decompressed: %d\n" (Bytes.length bytes);

  let s = decode_rle bytes |> Bytes.to_string in
  Printf.printf "Length rle decompressed: %d\n" (String.length s);

  let img = Image.create_rgb 320 200 in
  let ega_palette =
    [|0x0; 0xAA; 0xAA00; 0xAAAA; 0xAA0000; 0xAA00AA; 0xAA5500; 0xAAAAAA;
     0x555555; 0x5555FF; 0x55FF55; 0x55ffff; 0xff5555; 0xff55ff; 0xffff55; 0xffffff|]
  in
  let _ =
    String.fold (fun dim c ->
      match dim with
      | None -> None
      | Some (x, y) ->
        let c = int_of_char c in
        let h, l = c lsr 4, c land 0x0F in
        let write_color x index =
          let color = ega_palette.(index) in
          let r, g, b = color lsr 16, (color lsr 8) land 0xFF, color land 0xFF in
          (* Printf.printf "x:%d y:%d\n" x y; *)
          Image.write_rgb img x y r g b;
        in
        write_color x l;
        write_color (x+1) h;
        let x, y =
          if x + 2 >= 320 then 0, y+1 else x + 2, y
        in
        if y >= 200 then None else Some (x, y))
    (Some (0,0))
    s
  in
  let och = Png.chunk_writer_of_path destpath in
  ImagePNG.write och img

  (*
  IO.with_out "./temp.bin" @@
    fun ch -> Stdlib.output_bytes ch compressed;
  *)



