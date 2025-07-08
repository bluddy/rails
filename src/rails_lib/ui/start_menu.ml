open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

open Utils.Infix

type ('msg, 'state) t =
  | Action of {menu: ('msg, 'state) Menu.MsgBox.t; active: [`LoadGame | `StartGame]}
  | LoadGame of ('msg, 'state) Menu.MsgBox.t
  | Region of {menu: ('msg, 'state) Menu.MsgBox.t; active: Region.t }
  | Difficulty of {menu: ('msg, 'state) Menu.MsgBox.t; active: B_options.difficulty}
  | Options of {menu: ('msg, 'state) Menu.MsgBox.t; options: B_options.RealityLevels.t}

let action_menu fonts  =
  let open Menu.MsgBox in
  make ~draw_bg:false ~fonts
  [
    make_entry "Start new game" @@ `Action `NewGame;
    make_entry "Load a game" @@ `Action `LoadGame;
  ]

let region_menu fonts =
  let open Menu.MsgBox in
  let regions = Region.regions
    |> List.map (fun region ->
        let s = Region.show region in
        let year = Region.start_year region |> Int.to_string in
        region, Printf.sprintf "%s (%s)" s year)
  in
  let entries =
    List.map (fun (region, str) -> 
      make_entry ~select_action:region str @@ `Action region)
      regions
  in
  make ~draw_bg:false ~fonts entries

let difficulty_menu fonts =
  let open Menu.MsgBox in
  let entries = List.map (fun difficulty ->
      let str = B_options.show_difficulty difficulty in
      make_entry ~select_action:difficulty str @@ `Action difficulty)
    B_options.difficulties
  in
  make ~draw_bg:false ~fonts entries


