open! Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let sp = Printf.sprintf

type slot = {
  header: string option;
  slot: int;
}

type 'state t = {
  menu: (slot, 'state) Menu.MsgBox.t;
  action: [`Save | `Load];
}

module Header = struct
  type t = {
    version: int;
    save_title: string;
  } [@@deriving yojson]

  let title_of_str s = (Yojson.Safe.from_string s |> t_of_yojson ).save_title
end

let save_game_of_i i = sp "game%d.sav" i

let make_entries () = 
  let regex = Re.compile Re.(seq [str "game"; rep digit; str ".sav"]) in
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
        List.assoc ~eq:(=) i i_files
        |> fun s -> `Full (s, i)
      with
        Not_found -> `Empty i)
    Iter.(0 -- 9)
    |> Iter.to_list
  in
  let entries =
    List.map (function
      | `Full (file, i) ->
        let s = IO.File.read_exn file in
        let s = match String.split s ~by:"====" with
          | header::_ ->
              Header.title_of_str header
          | _ ->
              invalid_arg "bad header"
        in
        {header=Some s; slot=i}
      | `Empty i -> {header=None; slot=i})
    entries
  in
  entries

