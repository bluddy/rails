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
  let candidates = ref [] in
  let add dir =
    if not (List.mem ~eq:String.equal dir !candidates) then
      candidates := !candidates @ [dir]
  in
  (* 1. Explicit environment variable *)
  Option.iter add (get_env_opt "RAILS_ASSETS_DIR");
  (* 2. Current working directory *)
  add ".";
  (* 3. Next to executable *)
  add exe_dir;
  (* 4. Standard Linux / AppDir hierarchy (<exe_dir>/../share/rails) *)
  add (Filename.concat (Filename.concat exe_dir "..") "share/rails");
  add (Filename.concat (Filename.concat exe_dir "..") "share/games/rails");
  !candidates

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

let custom_data_dir = ref None

let set_data_dir path =
  custom_data_dir := Some path

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

let get_data_dir () =
  match !custom_data_dir with
  | Some d -> d
  | None ->
    let candidates = ref [] in
    let add dir =
      if not (List.mem ~eq:String.equal dir !candidates) then
        candidates := !candidates @ [dir]
    in
    (* 1. Explicit environment variable *)
    Option.iter add (get_env_opt "RAILS_DATA_DIR");
    (* 2. AppImage origin directory (if running as AppImage) *)
    (match get_env_opt "APPIMAGE" with
     | Some appimage_path ->
       add (Filename.concat (Filename.dirname appimage_path) "data")
     | None -> ());
    (match get_env_opt "OWD" with
     | Some owd -> add (Filename.concat owd "data")
     | None -> ());
    (* 3. Current working directory ./data *)
    add "data";
    add "./data";
    (* 4. Next to executable *)
    add (Filename.concat exe_dir "data");
    (* 5. Bundled fallback inside AppDir *)
    add (Filename.concat (Filename.concat exe_dir "..") "share/rails/data");
    (* 6. User data dir *)
    Option.iter add (user_data_dir ());

    let found = List.find_opt is_directory !candidates in
    match found with
    | Some d -> d
    | None -> "./data"

let resolve_data_path filename =
  let clean_name =
    if String.starts_with ~prefix:"./data/" filename then
      String.sub filename 7 (String.length filename - 7)
    else if String.starts_with ~prefix:"data/" filename then
      String.sub filename 5 (String.length filename - 5)
    else
      filename
  in
  let dir = get_data_dir () in
  Filename.concat dir clean_name

let data_file = resolve_data_path

let check_data_files required_files =
  let dir = get_data_dir () in
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
