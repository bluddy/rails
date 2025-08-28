open! Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives
module R = Renderer
module B = Backend
module C = Constants
module CS = Constants.Save

let version = 1

let num_slots = 10

let src = Logs.Src.create "savegame" ~doc:"Save_game"
module Log = (val Logs.src_log src: Logs.LOG)

include Save_game_d

open Utils.Infix

let sp = Printf.sprintf

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

let _make action (s:State.t) =
  let entries = make_entries () in
  let open Menu in
  let open MsgBox in
  let entries = List.map (fun entry ->
    let s = Option.get_or ~default:"EMPTY" entry.header in
    make_entry s @@ `Action(entry))
    entries
  in
  let menu =
    make ~fonts:s.fonts entries ~x:20 ~y:20
     |> Menu.MsgBox.do_open_menu s
  in
  {menu; action}

let make_save s = _make `Save s
let make_load s = _make `Load s

let render win (s:State.t) v =
  Menu.MsgBox.render win s v.menu

let save_title (s:State.t) =
  let b = s.backend in
  let name = B.get_name C.player b in
  let difficulty = B.get_difficulty b |> B_options.show_difficulty in
  sp "%s (%s) %d" name difficulty b.params.year

let save_game (s:State.t) slot =
  let to_string = Yojson.Safe.to_string in
  let header = {save_title=save_title s; version=CS.version} |> Header.yojson_of_t |> to_string in
  let backend = Backend.yojson_of_t s.backend |> to_string in
  let options = Main_ui_d.yojson_of_options s.ui.options |> to_string in
  let mapview = Mapview_d.yojson_of_t s.ui.view |> to_string in
  let s = String.concat "====" [header; backend; options; mapview] in
  let game_name = save_game_of_i slot in
  ignore(IO.File.write game_name s);
  print_endline @@ "Saved game to "^game_name^"."

(* Make state out of loaded game *)
let load_state backend ui_options ui_view win =
  let resources = Resources.load_all () in
  let region =  backend.Backend_d.params.region in
  let textures = Textures.of_resources win resources in
  let map_tex = R.Texture.make win @@ Tilemap.to_img backend.map in
  let map_silhouette_tex = R.Texture.make win @@ Tilemap.to_silhouette backend.map in
  let fonts = Fonts.load win in
  let ui = Main_ui.default ~options:ui_options ~view:ui_view win fonts region in
  {
    State.map_tex;
    map_silhouette_tex;
    mode=State.Game;
    backend;
    resources;
    textures;
    fonts;
    ui;
    win;
    random = Random.get_state ();
  }

let load_game slot win =
  let game_name = save_game_of_i slot in
  let s = IO.File.read_exn game_name in
  let lst = String.split s ~by:"====" in
  match lst with
  | [_header; backend; options; view] ->
      let from_string = Yojson.Safe.from_string in
      let backend = from_string backend |> Backend.t_of_yojson in
      let backend = {backend with pause = false} in
      Backend.reset_tick backend;
      let ui_options = from_string options |> Main_ui_d.options_of_yojson in
      let ui_view = from_string view |> Mapview_d.t_of_yojson in
      load_state backend ui_options ui_view win
  | _ -> failwith "Bad save game format"
  
let handle_event event (s:State.t) v =
  if Event.pressed_esc event then `Exit, v else
  match Menu.MsgBox.update s v.menu event with
  | menu2, Menu.On(entry) -> (* load entry *)
      let v = {v with menu=menu2} in
      let slot = entry.slot in
      begin match v.action with
      | `Load -> `LoadGame (load_game slot), v
      | `Save ->
          save_game s slot;
          `Exit, v
      end
  | menu2, _ when menu2 === v.menu -> `Stay, v
  | menu2, _ -> `Stay, {v with menu=menu2}
  


