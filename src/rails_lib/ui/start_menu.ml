open! Containers

module R = Renderer
module B = Backend
module C = Constants
module M = Money

include Start_menu_d

open Utils.Infix

let sp = Printf.sprintf

let action_menu fonts s =
  let x, y = 54, 39 in
  let open Menu.MsgBox in
  make ~x ~y ~font_idx:`Standard ~select_color:Ega.bcyan_transparent ~draw_bg:false ~border_x:0 ~use_prefix:false ~fonts
  [
    make_entry "Start new RR" @@ `Action(`NewGame);
    make_entry "Load Saved RR" @@ `Action(`LoadGame);
  ]
  |> do_open_menu s

let region_menu fonts s =
  let x, y = 54, 39 in
  let open Menu.MsgBox in
  let entries =
    List.map (fun region -> 
      let region_s = Region.show region in
      make_entry ~select_action:region region_s @@ `Action region)
      Region.regions
  in
  make ~x ~y ~font_idx:`Standard ~select_color:Ega.bcyan_transparent ~draw_bg:false ~border_x:0 ~use_prefix:false ~fonts entries
  |> do_open_menu s

let difficulty_menu fonts s =
  let x, y = 54, 39 in
  let open Menu.MsgBox in
  let entries = List.map (fun difficulty ->
      let str = B_options.show_difficulty difficulty in
      make_entry ~select_action:difficulty str @@ `Action difficulty)
    B_options.difficulties
  in
  make ~x ~y ~font_idx:`Standard ~select_color:Ega.bcyan_transparent ~draw_bg:false ~border_x:0 ~use_prefix:false ~fonts entries
  |> do_open_menu s

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
    | `Investor -> 1, 40, 91
    | `Financier -> 2, 100, 88
    | `Mogul -> 3, 169, 84
    | `Tycoon -> 4, 252, 86
    in
    let tex = Hashtbl.find s.textures.misc @@ `MainMenuMan idx in
    R.Texture.render win ~x ~y tex
  end;

  begin match v.mode with
  | Action menu ->
      write ~x:54 ~y:30 "Will you  ...";
      Menu.MsgBox.render win s menu
  | LoadGame state ->
      Load_game.render win s state
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

let handle_event (s:State.t) v (event:Event.t) time =
  let def = `Stay, v in
  let fonts = s.fonts in
  match v.mode with
  | Action menu ->
    let menu2, action = Menu.MsgBox.handle_event s menu event time in
    begin match action with
    | Menu.On(`NewGame) -> `Stay, {v with mode=Region(region_menu fonts s)}
    | Menu.On(`LoadGame) -> `Stay, {v with mode=LoadGame(Load_game.make s)}
    | _ when menu2 === menu -> def
    | _ -> `Stay, {v with mode=Action menu2}
    end

  | LoadGame state ->
      begin match Load_game.handle_event s state event time with
      | `LoadGame s, _ -> `LoadGame s, v
      | `Exit, _ -> `Stay, default s
      | `Stay, state2 when state2 === state -> `Stay, v
      | `Stay, state2 -> `Stay, {v with mode=LoadGame state2}
      end

  | Region menu ->
    let menu2, action = Menu.MsgBox.handle_event s menu event time in
    begin match action with
    | Menu.On(region) ->
        `Stay, {v with mode=Difficulty(difficulty_menu fonts s); region=Some region}
    | Menu.Selected(region) ->
        `Stay, {v with mode=Region menu2; region=Some region}
    | _ when menu2 === menu -> def
    | _ -> `Stay, {v with mode=Region menu2}
    end

  | Difficulty menu ->
    let menu2, action = Menu.MsgBox.handle_event s menu event time in
    begin match action with
    | Menu.On(difficulty) ->
        let reality = B_options.reality_levels_default in
        let mode = Reality{menu=reality_menu fonts reality s; reality} in
        `Stay, {v with mode; difficulty=Some difficulty}
    | Menu.Selected(difficulty) ->
        `Stay, {v with mode=Difficulty menu2; difficulty=Some difficulty}
    | _ when menu2 === menu -> def
    | _ -> `Stay, {v with mode=Difficulty menu2}
    end

  | Reality state ->
    let menu2, action = Menu.MsgBox.handle_event s state.menu event time in
    begin match action with
    | Menu.On `Continue ->
        let region = v.region |> Option.get_exn_or "missing region" in
        let difficulty = v.difficulty |> Option.get_exn_or "missing difficulty" in
        `Choose (region, difficulty, state.reality), v

    | Menu.On(#B_options.reality_level as reality_lvl) ->
        let reality = B_options.RealityLevels.toggle reality_lvl state.reality in
        let menu = reality_menu fonts reality s in
        `Stay, {v with mode=Reality {menu; reality}}
    | _ when menu2 === state.menu -> def
    | _ -> `Stay, {v with mode=Reality {state with menu=menu2}}
    end


