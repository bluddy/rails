open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

include Start_menu_d

open Utils.Infix

let sp = Printf.sprintf

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

let reality_menu fonts reality_set s =
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
    make_entry ops @@ `Action `DispatcherOps;
    make_entry economy @@ `Action `ComplexEconomy;
    make_entry competition @@ `Action `CutthroatCompetition;
  ] |> do_open_menu s

let default s =
  let menu = action_menu s.State.fonts s in
  { mode=Action menu; region=None; difficulty=None }

let render win v (s:State.t) = 
  let fonts = s.fonts in
  let write = Fonts.Render.write win fonts ~color:Ega.black in
  let write_caps = write ~idx:`Caps in
  let write_large = write ~idx:`Large in
  let write = write ~idx:`Standard in
  let bg_tex = Hashtbl.find s.textures.misc `MainMenuBackground in
  R.clear_screen win;
  R.Texture.render win ~x:0 ~y:0 bg_tex;
  begin match v.region with
  | None -> ()
  | Some region ->
      let tex = Hashtbl.find s.textures.misc @@ `MainMenu region in
      R.Texture.render win ~x:182 ~y:22 tex;
  end;

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
  | Action menu ->
      write ~x:54 ~y:30 "Will you  ...";
      Menu.MsgBox.render win s menu
  | LoadGame -> ()
  | Region menu ->
      write ~x:54 ~y:30 "Select area...";
      Menu.MsgBox.render win s menu
  | Difficulty menu ->
      write ~x:54 ~y:30 "Are you an...";
      Menu.MsgBox.render win s menu;
  | Reality state ->
      Menu.MsgBox.render win s state.menu;
      write_caps ~x:58 ~y:30 "Difficulty\nFactor:";
      let difficulty = Option.get_exn_or "difficulty" v.difficulty in
      let pct = B_options.compute_difficulty_pct difficulty state.reality in
      write_large ~x:58 ~y:46 @@ sp "%d%%" pct;
      write_caps ~x:58 ~y:70 @@ sp "(%s)" @@ B_options.show_difficulty difficulty;
  end;
  ()

let handle_event (s:State.t) v (event:Event.t) =
  let default = `None, v in
  let fonts = s.fonts in
  match v.mode with
  | Action menu ->
    let menu2, action = Menu.MsgBox.update s menu event in
    begin match action with
    | Menu.On(`NewGame) -> `None, {v with mode=Region(region_menu fonts s)}
    | Menu.On(`LoadGame) -> `None, {v with mode=LoadGame}
    | _ when menu2 === menu -> default
    | _ -> `None, {v with mode=Action menu2}
    end

  | LoadGame -> default

  | Region menu ->
    let menu2, action = Menu.MsgBox.update s menu event in
    begin match action with
    | Menu.On(region) ->
        `None, {v with mode=Difficulty(difficulty_menu fonts s); region=Some region}
    | Menu.Selected(region) ->
        `None, {v with mode=Region menu2; region=Some region}
    | _ when menu2 === menu -> default
    | _ -> `None, {v with mode=Region menu2}
    end

  | Difficulty menu ->
    let menu2, action = Menu.MsgBox.update s menu event in
    begin match action with
    | Menu.On(difficulty) ->
        let reality = B_options.reality_levels_default in
        let mode = Reality{menu=reality_menu fonts reality s; reality} in
        `None, {v with mode; difficulty=Some difficulty}
    | Menu.Selected(difficulty) ->
        `None, {v with mode=Difficulty menu2; difficulty=Some difficulty}
    | _ when menu2 === menu -> default
    | _ -> `None, {v with mode=Difficulty menu2}
    end

  | Reality state ->
    let menu2, action = Menu.MsgBox.update s state.menu event in
    begin match action with
    | Menu.On `Continue ->
        `Choose (v.region, v.difficulty, state.reality), v
        
    | Menu.On(#B_options.reality_level as reality_lvl) ->
        let reality = B_options.RealityLevels.toggle reality_lvl state.reality in
        let menu = reality_menu fonts reality s in
        `None, {v with mode=Reality {menu; reality}}
    | _ when menu2 === state.menu -> default
    | _ -> `None, {v with mode=Reality {state with menu=menu2}}
    end


