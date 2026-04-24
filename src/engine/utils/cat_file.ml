
(*
CAT format

0 | 16 bits | number of entries
2 | N entries
*)

module G = My_gen
let of_file ?(debug=true) ?(dump=false) file =
  let s = Utils.stream_of_file file in
  let num_pics = My_gen.get_wordi s in
  let rec parse_header s n l =
    if n <= 0 then l else
    let filename = Gen.take 12 s |> My_gen.to_stringi |> Utils.remove_nulls in
    let data1 = My_gen.get_wordi s in 
    let data2 = My_gen.get_wordi s in 
    let data3 = My_gen.get_wordi s in (* This is the compressed size *)
    let data = [|data1;data2;data3|] in
    My_gen.junki ~num:2 s;
    let offset = My_gen.get_wordi s in
    My_gen.junki ~num:2 s;
    if debug then
      Printf.printf "filename: %s, offset:0x%x, data: 0x%x 0x%x 0x%x\n" filename offset (data.(0)) (data.(1)) (data.(2));
    parse_header s (n-1) @@ (filename, data, offset)::l
  in
  let file_header = parse_header s num_pics [] |> List.rev in
  List.map (fun (filename, data, offset) ->
    let compressed_size = data.(2) in
    Printf.printf "opening sub-file %s at 0x%x (size: 0x%x)\n" filename offset compressed_size;
    
    (* Create a stream that is EXACTLY the size of the compressed sub-file *)
    let full_stream = Utils.stream_of_file_seek offset file in
    let limited_stream = Gen.take compressed_size full_stream in
    
    let ndarray = Pic.interpret_stream limited_stream in
    
    if dump then (
      let out_name = Filename.chop_extension filename ^ ".png" in
      Pic.png_of_ndarray ~filename:out_name ndarray;
    );
    filename, Pic.img_of_ndarray ~transparent:true ndarray
  )
  file_header
