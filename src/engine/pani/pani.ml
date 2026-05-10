open Containers

let src = Logs.Src.create "pani" ~doc:"Pani"
module Log = (val Logs.src_log src: Logs.LOG)

let of_stream ?debug ?input ?(dump=false) s =
  let pani = Gen.take 4 s |> My_gen.to_stringi in
  if String.(pani = "PANI")
  then ()
  else failwith "Not a PANI file";
  let _pani_byte1 = My_gen.get_bytei s in
  let pani_lzw_encoded = My_gen.get_bytei s in
  let pani_byte3 = My_gen.get_bytei s in
  Log.debug (fun f -> f "lzw_encoded: 0x%x\nbyte3: 0x%x\n" pani_lzw_encoded pani_byte3); (* debug *)
  let header_type = My_gen.get_bytei s in
  Log.debug (fun f -> f "header_type: 0x%x\n" header_type);
  let _metadata =
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
  Log.debug (fun f -> f "pani_type: 0x%x\n" pani_type);

  let pani_pics = Array.make 251 None in

  let _ega_params = if pani_type = 2 then (
    My_gen.get_wordi s
  ) else 0 in

  let pic_bgnd = match pani_type with
    | 0 -> None
    | 1 ->

        Log.debug (fun f -> f "Loading background image\n");

        let ndarray = Pic.interpret_stream s in
        (* Doesn't needs to be transparent, but still stored that way *)
        let pic_bgnd = Some(Pic.img_of_ndarray ~transparent:true ndarray) in

        if dump then Pic.png_of_ndarray ndarray ~filename:"bgnd.png";
        pic_bgnd
    | 2 -> None
    | _ -> failwith "Unknown value for pani_type"
  in

  let align_pos () =
    let pos = My_gen.pos () + 1 in
    if pos land 1 = 1 then (
      Log.debug (fun f -> f "junking odd position");
      My_gen.junki s
    )
  in

  (* HACK (not in source) to adjust if we're not word aligned *)
  align_pos ();

  (* Support up to 250 images, lined up towards end, zeros before then *)
  let pani_pic_ptrs = Array.make 250 0 in
  Log.debug (fun f -> f "Post-Background pos: 0x%x\n" (My_gen.pos () + 1));

  print_endline "--- PANI pics ---";
  for i=0 to 249 do
    let word = My_gen.get_wordi s in
    Log.debug (fun f -> f "%d: 0x%x\n" i word);
    pani_pic_ptrs.(i) <- word
  done;
  let num = Array.fold (fun acc x -> if x = 0 then acc else acc + 1) 0 pani_pic_ptrs in
  Log.debug (fun f -> f "%d pictures expected\n" num);

  Array.iteri (fun i x ->
    match x with
    | 0 -> ()
    | _ ->
        let pos = My_gen.pos () + 1 in
        Log.debug (fun f -> f "pos: 0x%x\n" pos);
        (* We can only start at word boundaries *)
        align_pos ();
        Log.debug (fun f -> f "Load pic. Idx: %d. Pos: 0x%x.\n" i (My_gen.pos () + 1));
        let ndarray = Pic.interpret_stream s in
        pani_pics.(i) <- Some(Pic.img_of_ndarray ~transparent:true ndarray);

        if dump then Pic.png_of_ndarray ndarray ~filename:(Printf.sprintf "%d_0x%x.png" i pos);
  )
  pani_pic_ptrs;

  (* Animation interpreter code *)
  align_pos ();
  let pos = My_gen.pos () + 1 in
  let size_ending = My_gen.get_wordi s in
  Log.debug (fun f -> f "0x%x: %d 16-byte entries\n" pos size_ending);

  let pani_code_s = My_gen.to_stringi s |> Bytes.of_string in

  if dump then (
      let f = open_out "code.txt" in
      output_bytes f pani_code_s;
      close_out f
  );

  let pani_v = Pani_interp.make ?debug ?input pani_code_s pic_bgnd pani_pics in
  pani_v

let dump_file filename =
  (* For dumping *)
  let stream = Utils.stream_of_file filename in
  let pani_v = of_stream stream ~dump:true in
  Pani_interp.dump_run_to_end pani_v

