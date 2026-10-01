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
  error_modal: (unit, 'state) Menu.MsgBox.t option;
}

module Header = struct
  type t = {
    version: int [@default 1];
    save_title: string;
  } [@@deriving yojson]

  let of_string str =
    try
      Ok (Yojson.Safe.from_string str |> t_of_yojson)
    with exn ->
      Error (Printexc.to_string exn)

  let of_string_opt str =
    match of_string str with
    | Ok h -> Some h
    | Error _ -> None

  let title_of_str s =
    match of_string s with
    | Ok h -> h.save_title
    | Error _ -> "[CORRUPTED SAVE]"
end

let save_game_of_i i = sp "game%d.sav" i

let make_entries () = 
  let regex = Re.compile Re.(seq [str "game"; group @@ rep digit; str ".sav"]) in
  let files = IO.File.read_dir @@ IO.File.make "./" in
  let save_files = Gen.filter (fun s ->
    try ignore @@ Re.exec regex s; true
    with Not_found -> false) files 
    |> Gen.to_list
  in
  let i_files = List.map (fun s -> Re.exec regex s
    |> (fun grp -> Re.Group.get grp 1)
    |> Int.of_string_exn, s) save_files in
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
        let s =
          match Save_compression.read_save_file file with
          | Error _ -> "[CORRUPTED SAVE]"
          | Ok raw ->
            match String.split raw ~by:"====" with
            | header::_ -> Header.title_of_str header
            | _ -> "[CORRUPTED SAVE]"
        in
        {header=Some s; slot=i}
      | `Empty i -> {header=None; slot=i})
    entries
  in
  entries

