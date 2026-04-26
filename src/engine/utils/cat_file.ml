
let of_file ?(debug=false) ?(dump=false) file =
  let s = Utils.stream_of_file file in
  let num_pics = My_gen.get_wordi s in

  let rec parse_header s n l =
    if n <= 0 then l else
    let filename = Gen.take 12 s |> My_gen.to_stringi |> Utils.remove_nulls in
    let data1 = My_gen.get_wordi s in 
    let data2 = My_gen.get_wordi s in 
    let data3 = My_gen.get_wordi s in (* Compressed Size *)
    let data = [|data1;data2;data3|] in
    My_gen.junki ~num:2 s;
    let offset = My_gen.get_wordi s in
    My_gen.junki ~num:2 s;

    if debug then
      Printf.printf "Header found - %s: Offset: 0x%x, Size: 0x%x\n" filename offset data3;

    parse_header s (n-1) @@ (filename, data, offset)::l
  in

  let file_header = parse_header s num_pics [] |> List.rev in

  List.map (fun (filename, data, offset) ->
    let compressed_size = data.(2) in

    (* 1. RAW DUMP LOGIC *)
    if dump then (
      Printf.printf "Dumping raw sub-file: %s...\n" filename;
      (* Create a fresh stream from the offset *)
      let raw_s = Utils.stream_of_file_seek offset file in
      (* Take exactly the compressed size bytes *)
      let raw_bytes = Gen.take compressed_size raw_s |> My_gen.to_stringi in

      (* Write to a standalone .pic file *)
      let out_channel = open_out_bin filename in
      output_string out_channel raw_bytes;
      close_out out_channel;
      Printf.printf "Created standalone file: %s\n" filename;
    );

    (* 2. PROCEED TO DECOMPRESSION *)
    Printf.printf "Processing %s...\n" filename;
    let s = Utils.stream_of_file_seek offset file in
    let limited_s = Gen.take compressed_size s in
    let ndarray = Pic.interpret_stream limited_s in

    filename, Pic.img_of_ndarray ~transparent:true ndarray
  )
  file_header
