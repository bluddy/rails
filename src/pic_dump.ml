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
        rle := false
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
  let rle_bytes =
    IO.with_in filename @@
      fun in_channel -> IO.read_all_bytes in_channel
  in
  let bytes = decode_rle rle_bytes in
  let compressed = int_list_of_bytes bytes in

  let decompressed = Lzw.decompress compressed
    |> String.concat ""
  in

  print_endline "--- Pic dump";

  Printf.printf "Length original: %d\n" (Bytes.length rle_bytes);
  Printf.printf "Length rle decompressed: %d\n" (Bytes.length bytes);

  Printf.printf "Length compressed: %d\n" (List.length compressed);
  Printf.printf "Length decompressed: %d\n" (String.length decompressed);
  ()

let () =
  main Sys.argv.(1)



