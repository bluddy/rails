open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

open Utils.Infix

let sp = Printf.sprintf

type ('msg, 'state) mode =
  | Action of ([`NewGame | `LoadGame] option, 'state) Menu.MsgBox.t
  | LoadGame of ('msg, 'state) Menu.MsgBox.t
  | Region of (Region.t, 'state) Menu.MsgBox.t
  | Difficulty of {menu: (B_options.difficulty, 'state) Menu.MsgBox.t}
  | Options of {menu: ('msg, 'state) Menu.MsgBox.t; options: B_options.RealityLevels.t}

type ('msg, 'state) t = {
  mode: ('msg, 'state) mode;
  cur_region: Region.t option;
  difficulty: B_options.difficulty option;
}

let x, y = 54, 42

let action_menu fonts s =
  let open Menu.MsgBox in
  make ~x ~y ~draw_bg:false ~fonts
  [
    make_entry "Start new game" @@ `Action(`NewGame);
    make_entry "Load a game" @@ `Action(`LoadGame);
  ]
  |> do_open_menu s

let region_menu fonts s =
  let open Menu.MsgBox in
  let entries =
    List.map (fun region -> 
      let region_s = Region.show region in
      let year = Region.start_year region |> Int.to_string in
      let str = Printf.sprintf "%s (%s)" region_s year in
      make_entry ~select_action:region str @@ `Action region)
      Region.regions
  in
  make ~x ~y ~draw_bg:false ~fonts entries |> do_open_menu s

let difficulty_menu fonts s =
  let open Menu.MsgBox in
  let entries = List.map (fun difficulty ->
      let str = B_options.show_difficulty difficulty in
      make_entry ~select_action:difficulty str @@ `Action difficulty)
    B_options.difficulties
  in
  make ~x ~y ~draw_bg:false ~fonts entries |> do_open_menu s

let reality_menu fonts reality_set =
  let open Menu.MsgBox in
  let module RSet = B_options.RealityLevels in
  let pos = RSet.mem reality_set `DispatcherOps in
  let ops = B_options.show_reality_level `DispatcherOps ~pos in
  let pos = RSet.mem reality_set `ComplexEconomy in
  let economy = B_options.show_reality_level `ComplexEconomy ~pos in
  let pos = RSet.mem reality_set `CutthroatCompetition in
  let competition = B_options.show_reality_level `CutthroatCompetition ~pos in
  make ~x:80 ~y:80 ~fonts ~heading:"Change reality levels ?" [
    make_entry "Continue" @@ `Action `Continue;
    make_entry ops @@ `Action `OpsToggle;
    make_entry economy @@ `Action `EconomyToggle;
    make_entry competition @@ `Action `CompetitionToggle;
  ]

let default fonts =
  let menu = action_menu fonts in
  menu

let render win v (s:State.t) = 
  let fonts = s.fonts in
  let write = Fonts.Render.write win fonts ~color:Ega.black in
  let write_caps = write ~idx:`Caps in
  let write_large = write ~idx:`Large in
  let write = write ~idx:`Standard in
  let bg_tex = Hashtbl.find s.textures.misc `MainMenuBackground in
  R.clear_screen win;
  R.Texture.render win ~x:0 ~y:0 bg_tex;
  let tex = match v.cur_region with
  | None -> Hashtbl.find s.State.textures.misc `Logo
  | Some region -> Hashtbl.find s.textures.misc @@ `MainMenu region
  in
  R.Texture.render win ~x:182 ~y:22 tex;

  (* draw man *)
  begin match v.difficulty with
  | None -> ()
  | Some active_man ->
    let idx, x, y = match active_man with
    | `Investor -> 0, 40, 90
    | `Financier -> 1, 104, 90
    | `Mogul -> 2, 166, 80
    | `Tycoon -> 3, 230, 84
    in
    let tex = Hashtbl.find s.textures.misc @@ `MainMenuMan idx in
    R.Texture.render win ~x ~y tex
  end;

  begin match v.mode with
  | Action state ->
      write ~x:54 ~y:30 "Will you  ...";
      Menu.MsgBox.render win s state
  | LoadGame state ->
      Menu.MsgBox.render win s state
  | Region menu ->
      write ~x:54 ~y:30 "Select area...";
      Menu.MsgBox.render win s menu
  | Difficulty v ->
      write ~x:54 ~y:30 "Are you an...";
      Menu.MsgBox.render win s v.menu;
  | Options state ->
      Menu.MsgBox.render win s state.menu;
      write_caps ~x:58 ~y:30 "Difficulty\nFactor:";
      let difficulty = Option.get_exn_or "difficulty" v.difficulty in
      let pct = B_options.compute_difficulty_pct difficulty state.options in
      write_large ~x:58 ~y:46 @@ sp "%d%%" pct;
      write_caps ~x:58 ~y:70 @@ sp "(%s)" @@ B_options.show_difficulty difficulty;
  end;
  ()

