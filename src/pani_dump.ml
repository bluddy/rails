open Containers

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
        for i = 0 to x - 2 do
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
  Printf.printf "--- PANI dump: %s\n" filename;

  let filepath = Filename.remove_extension filename in
  let destpath = filepath ^ ".png" in

  let bytes =
    IO.with_in filename @@
      fun in_channel -> IO.read_all_bytes in_channel
  in
  let offset = 0x2B in
  let start_offset =
    match Bytes.get_uint16_le bytes 0x24 with
    | 0x7 -> offset
    | 0x2007 -> offset
    | x -> failwith @@ Printf.sprintf "Unknown format %x" x
  in
  let width = Bytes.get_uint16_le bytes 0x26 in
  let height = Bytes.get_uint16_le bytes 0x28 in
  Printf.printf "width=%d, height=%d, total_size=%d\n" width height (width*height/2);
  Printf.printf "Length original: %d\n" (Bytes.length bytes);
  let bytes = Bytes.sub bytes start_offset (Bytes.length bytes - start_offset) in

  let bytes = Lzw.decompress bytes ~max_bit_size:11 ~report_offset:offset ~suppress_error:true in
  Printf.printf "Length LZW decompressed: %d\n" (Bytes.length bytes);

  let s = decode_rle bytes |> Bytes.to_string in
  Printf.printf "Length rle decompressed: %d\n" (String.length s);

  let img = Image.create_rgb width height in
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
          if x + 2 >= width then 0, y+1 else x + 2, y
        in
        if y >= height then None else Some (x, y))
    (Some (0,0))
    s
  in
  let och = Png.chunk_writer_of_path destpath in
  ImagePNG.write och img

  (*
  IO.with_out "./temp.bin" @@
    fun ch -> Stdlib.output_bytes ch compressed;
  *)



