open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

open Utils.Infix

type ('msg, 'state) t =
  | Action of {menu: ([`NewGame | `LoadGame], 'state) Menu.MsgBox.t}
  | LoadGame of ('msg, 'state) Menu.MsgBox.t
  | Region of {menu: (Region.t, 'state) Menu.MsgBox.t; active: Region.t }
  | Difficulty of {menu: (B_options.difficulty, 'state) Menu.MsgBox.t; active: B_options.difficulty}
  | Options of {menu: ('msg, 'state) Menu.MsgBox.t; options: B_options.RealityLevels.t}

let action_menu fonts  =
  let open Menu.MsgBox in
  make ~draw_bg:false ~fonts
  [
    make_entry "Start new game" @@ `Action `NewGame;
    make_entry "Load a game" @@ `Action `LoadGame;
  ]
  |> do_open_menu

let region_menu fonts =
  let open Menu.MsgBox in
  let entries =
    List.map (fun region -> 
      let region_s = Region.show region in
      let year = Region.start_year region |> Int.to_string in
      let str = Printf.sprintf "%s (%s)" region_s year in
      make_entry ~select_action:region str @@ `Action region)
      Region.regions
  in
  make ~draw_bg:false ~fonts entries |> do_open_menu

let difficulty_menu fonts =
  let open Menu.MsgBox in
  let entries = List.map (fun difficulty ->
      let str = B_options.show_difficulty difficulty in
      make_entry ~select_action:difficulty str @@ `Action difficulty)
    B_options.difficulties
  in
  make ~draw_bg:false ~fonts entries |> do_open_menu

let reality_menu fonts = ()

let default fonts =
  let menu = action_menu fonts in
  Action {menu}

let render win v (s:State.t) = 
  let bg_tex = Hashtbl.find s.textures `MainMenuBackground in
  R.clear_screen win;
  R.Texture.render win ~x:0 ~y:0 bg_tex;
  begin match v with
  | Action v ->
      Menu.MsgBox.render win s v.menu
  | LoadGame v ->
      Menu.MsgBox.render win s v
  | Region v ->
      Menu.MsgBox.render win s v.menu
  | Difficulty v ->
      Menu.MsgBox.render win s v.menu
  | Options v ->
      Menu.MsgBox.render win s v.menu
  end;
  ()



