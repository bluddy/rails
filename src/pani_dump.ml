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




