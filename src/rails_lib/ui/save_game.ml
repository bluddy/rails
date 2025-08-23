open! Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let num_slots = 10

let src = Logs.Src.create "savegame" ~doc:"Save_game"
module Log = (val Logs.src_log src: Logs.LOG)

let sp = Printf.sprintf

module Header = struct
  type t = {
    version: int;
    save_title: string;
  } [@@deriving yojson]

  let title_of_str s = (Yojson.Safe.from_string s |> t_of_yojson ).save_title

end

let make_entries () = 
  let regex = Re.compile Re.(seq [str "game_"; rep digit; str ".sav"]) in
  let files = IO.File.read_dir @@ IO.File.make "./" in
  let save_files = Gen.filter (fun s ->
    try ignore @@ Re.exec regex s; true
    with Not_found -> false) files 
    |> Gen.to_list
  in
  let i_files = List.map (fun s -> Re.matches regex s |> List.hd |> Int.of_string_exn, s) save_files in
  let entries =
    Iter.map (fun i ->
      try
        List.assoc ~eq:(=) i i_files |> Option.some
      with
        Not_found -> None)
    Iter.(0 -- 9)
    |> Iter.to_list
  in
  let entries =
    List.map (Option.map @@
     fun file ->
       let s = IO.File.read_exn file in
       match String.split s ~by:"====" with
         | header::_ ->
             Header.title_of_str header
         | _ ->
             invalid_arg "bad header"
    )
    entries
  in
  entries

