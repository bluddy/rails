
(*
CAT format

0 | 16 bits | number of entries
2 | N entries
*)

module G = My_gen

let of_stream ?(debug=true) ?(dump=false) s =
  let num_pics = My_gen.get_wordi s in
  let rec parse_header s n l =
    if n <= 0 then l else
    let filename = Gen.take 12 s |> My_gen.to_stringi |> Utils.remove_nulls in
    if debug then print_endline filename;
    let _data = Gen.take 6 s |> My_gen.to_stringi in
    My_gen.junki ~num:2 s;
    let offset = My_gen.get_wordi s in
    My_gen.junki ~num:2 s;
    parse_header s (n-1) @@ (filename, offset)::l
  in
  let file_header = parse_header s num_pics [] |> List.rev in
  List.map (fun (filename, offset) ->
    Printf.printf "offset:0x%x, pos:0x%x\n" offset (My_gen.pos ());
    let ndarray = Pic.interpret_stream s in
    if dump then (
      let filename = Filename.chop_extension filename ^ ".png" in
      Pic.png_of_ndarray ~filename ndarray;
    );
    filename, Pic.img_of_ndarray ~transparent:true ndarray
  )
  file_header

