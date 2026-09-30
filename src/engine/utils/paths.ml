open Containers

let exe_dir =
  try
    let exe = Sys.executable_name in
    let abs_exe =
      if Filename.is_relative exe then
        Filename.concat (Sys.getcwd ()) exe
      else
        exe
    in
    Filename.dirname abs_exe
  with _ -> "."

let get_env_opt var =
  try
    let v = Sys.getenv var in
    let trimmed = String.trim v in
    if String.equal trimmed "" then None else Some trimmed
  with Not_found -> None

let is_directory dir =
  try Sys.is_directory dir with _ -> false

let file_exists path =
  try Sys.file_exists path with _ -> false

(** Candidate directories to search for assets (shaders, sound, music) *)
let asset_search_dirs () =
  let env_dir = Option.to_list (get_env_opt "RAILS_ASSETS_DIR") in
  let static_dirs = [
    ".";
    exe_dir;
    Filename.concat (Filename.concat exe_dir "..") "share/rails";
    Filename.concat (Filename.concat exe_dir "..") "share/games/rails";
  ] in
  List.uniq ~eq:String.equal (env_dir @ static_dirs)

let find_asset_dir sub_dir =
  match get_env_opt (Printf.sprintf "RAILS_%s_DIR" (String.uppercase_ascii sub_dir)) with
  | Some d when is_directory d -> d
  | _ ->
    let dirs = asset_search_dirs () in
    let found =
      List.find_opt
        (fun base -> is_directory (Filename.concat base sub_dir))
        dirs
    in
    match found with
    | Some base -> Filename.concat base sub_dir
    | None -> sub_dir

let find_asset_file sub_path =
  let dirs = asset_search_dirs () in
  let found =
    List.find_opt
      (fun base -> file_exists (Filename.concat base sub_path))
      dirs
  in
  match found with
  | Some base -> Filename.concat base sub_path
  | None -> sub_path

let user_data_dir () =
  if Sys.win32 then
    match get_env_opt "APPDATA" with
    | Some appdata -> Some (Filename.concat appdata "Rails/data")
    | None -> None
  else if String.equal Sys.os_type "Unix" then
    match get_env_opt "HOME" with
    | Some home when file_exists (Filename.concat home "Library/Application Support") ->
      Some (Filename.concat home "Library/Application Support/Rails/data")
    | Some home ->
      let base = match get_env_opt "XDG_DATA_HOME" with
        | Some xdg -> xdg
        | None -> Filename.concat (Filename.concat home ".local") "share"
      in
      Some (Filename.concat (Filename.concat base "rails") "data")
    | None -> None
  else None

let data_search_dirs () =
  let appimage_data =
    Option.map
      (fun p -> Filename.concat (Filename.dirname p) "data")
      (get_env_opt "APPIMAGE")
  in
  let owd_data =
    Option.map
      (fun owd -> Filename.concat owd "data")
      (get_env_opt "OWD")
  in
  let candidates =
    List.filter_map Fun.id [
      appimage_data;
      owd_data;
      Some "data";
      Some "./data";
      Some (Filename.concat exe_dir "data");
      Some (Filename.concat (Filename.concat exe_dir "..") "share/rails/data");
      user_data_dir ();
    ]
  in
  List.uniq ~eq:String.equal candidates

let get_data_dir ?custom_dir () =
  match custom_dir with
  | Some d -> d
  | None ->
    match get_env_opt "RAILS_DATA_DIR" with
    | Some d -> d
    | None ->
      let dirs = data_search_dirs () in
      match List.find_opt is_directory dirs with
      | Some d -> d
      | None -> "./data"

let resolve_data_path ?data_dir filename =
  let clean_name =
    if String.starts_with ~prefix:"./data/" filename then
      String.sub filename 7 (String.length filename - 7)
    else if String.starts_with ~prefix:"data/" filename then
      String.sub filename 5 (String.length filename - 5)
    else
      filename
  in
  let dir = match data_dir with
    | Some d -> d
    | None -> get_data_dir ()
  in
  Filename.concat dir clean_name

let data_file = resolve_data_path

let check_data_files ?data_dir required_files =
  let dir = match data_dir with
    | Some d -> d
    | None -> get_data_dir ()
  in
  if not (is_directory dir) then
    Error ("data directory", dir)
  else
    let missing =
      List.find_opt (fun file ->
        let path = Filename.concat dir file in
        not (file_exists path))
        required_files
    in
    match missing with
    | Some f -> Error (f, dir)
    | None -> Ok ()

let report_fatal_error ~title ~message =
  prerr_endline (Printf.sprintf "\n========================================\nERROR: %s\n\n%s\n========================================\n" title message);
  let _ =
    try
      let _ = Tsdl.Sdl.init Tsdl.Sdl.Init.video in
      Tsdl.Sdl.show_simple_message_box Tsdl.Sdl.Message_box.error ~title message None
    with _ -> Ok ()
  in
  ()
