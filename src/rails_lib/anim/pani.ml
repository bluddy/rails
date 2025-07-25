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


let of_stream ?input ?(dump_files=None) s =
  let pani = Gen.take 4 s |> My_gen.to_stringi in
  if String.(pani = "PANI")
  then ()
  else failwith "Not a PANI file";
  let _pani_byte1 = My_gen.get_bytei s in
  let pani_lzw_encoded = My_gen.get_bytei s in
  let pani_byte3 = My_gen.get_bytei s in
  Printf.printf "lzw_encoded: 0x%x\nbyte3: 0x%x\n" pani_lzw_encoded pani_byte3; (* debug *)
  let header_type = My_gen.get_bytei s in
  Printf.printf "header_type: 0x%x\n" header_type; (* debug *)
  let _subheader =
    match header_type with
    | 0 -> Gen.take 17 s |> My_gen.to_stringi
    | 1 -> ""
    | 2 -> Gen.take 774 s |>  My_gen.to_stringi
    | n -> failwith @@ Printf.sprintf "Bad header_type %d" n
  in
  let pani_struct = Vector.create () in (* Actually 9 words *)
  Vector.push pani_struct (My_gen.get_wordi s);
  Vector.push pani_struct (My_gen.get_wordi s);
  Vector.push pani_struct (My_gen.get_wordi s);
  Vector.push pani_struct (My_gen.get_wordi s);
  let _pani_word = My_gen.get_wordi s in
  (* pani_read_buffer_2 *)
  let pani_type = My_gen.get_bytei s in
  Printf.printf "pani_type: 0x%x\n" pani_type; (* debug *)

  let pani_pics = Array.make 251 None in

  let pic_bgnd =
    match pani_type with
    | 0 -> None
    | 1 ->
        (* let byte = My_gen.get_bytei s in  (* optional *)
        Printf.printf "byte: 0x%x pos: 0x%x\n" byte (My_gen.pos ());  *)

        Printf.printf "Loading background image\n";

        let ndarray = Pic.ndarray_of_stream s in
        (* Doesn't needs to be transparent, but still stored that way *)
        let pic_bgnd = Some(Pic.img_of_ndarray ~transparent:true ndarray) in

        begin match dump_files with
        | Some filepath ->
          Pic.png_of_ndarray ndarray ~filename:(Printf.sprintf "%s_bgnd.png" filepath)
        | None -> ()
        end;
        pic_bgnd
    | 2 -> None
    | _ -> failwith "Unknown value for pani_type"
  in

  let align_pos () =
    let pos = My_gen.pos () + 1 in
    if pos land 1 = 1 then (
      print_endline "junking odd position";
      My_gen.junki s
    )
  in

  (* HACK (not in source) to adjust if we're not word aligned *)
  align_pos ();

  (* Support up to 250 images, lined up towards end, zeros before then *)
  let pani_pic_ptrs = Array.make 250 0 in
  Printf.printf "Post-Background pos: 0x%x\n" (My_gen.pos () + 1);

  for i=0 to 249 do
    let word = My_gen.get_wordi s in
    (* Printf.printf "%d: 0x%x\n" i word; *)
    pani_pic_ptrs.(i) <- word
  done;
  let num = Array.fold (fun acc x -> if x = 0 then acc else acc + 1) 0 pani_pic_ptrs in
  Printf.printf "%d pictures expected\n" num;

  Array.iteri (fun i x ->
    match x with
    | 0 -> ()
    | _ ->
        let pos = My_gen.pos () + 1 in
        Printf.printf "pos: 0x%x\n" pos;
        (* We can only start at word boundaries *)
        align_pos ();
        Printf.printf "Load pic. Idx: %d. Pos: 0x%x.\n" i (My_gen.pos () + 1);
        let ndarray = Pic.ndarray_of_stream s in
        pani_pics.(i) <- Some(Pic.img_of_ndarray ~transparent:true ndarray);
        
        match dump_files with
        | Some filepath ->
              Pic.png_of_ndarray ndarray ~filename:(Printf.sprintf "%s_%d.png" filepath i)
        | None -> ()
  )
  pani_pic_ptrs;

  (* Animation interpreter code *)
  align_pos ();
  let pos = My_gen.pos () + 1 in
  let size_ending = My_gen.get_wordi s in
  Printf.printf "0x%x: %d 16-byte entries\n" pos size_ending;
  (* fill with words for now *)
  (*
  let pani_arr = Array.make (size_ending * 8) 0 in
  for i=0 to size_ending * 8 - 1 do
    pani_arr.(i) <- My_gen.get_wordi s;
    Printf.printf "0x%x: 0x%x\n" (My_gen.pos ()) pani_arr.(i);
  done
  *)
  let pani_code_s = My_gen.to_stringi s |> Bytes.of_string in

  begin match dump_files with
  | Some filepath ->
      let out_file = Printf.sprintf "%s_code.txt" filepath in
      let f = open_out out_file in
      output_bytes f pani_code_s;
      close_out f
  | None -> ()
  end;

  let pani_v = Pani_interp.make ?input pani_code_s pic_bgnd pani_pics in
  pani_v

let stream_of_file filename =
  let str =
    IO.with_in filename @@
      fun in_channel -> IO.read_all in_channel
  in
  let stream = My_gen.of_stringi str in
  stream

let play ?(debug=false) filename =
  let stream = stream_of_file filename in
  let dump_files =
    if debug then
      let filepath = Filename.remove_extension filename in
      Some filepath
    else None
  in
  let pani_v = of_stream stream ~dump_files in
  Pani_interp.run_to_end pani_v

