open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer
module B = Backend
module Random = Utils.Random
open Utils.Infix

let update_map _win v map =
  R.Texture.update v.State.map_tex @@ Tilemap.to_img map

let default_state win sound : State.t =
  let resources = Resources.load_all () in
  let textures = Textures.of_resources win resources in
  let fonts = Fonts.load win in
  let backend = Backend.default in
  let map_tex = Hashtbl.find textures.misc `Advert in
  let ui = Main_ui.default win fonts Region.WestUS in
  {
    map_tex;
    map_silhouette_tex=map_tex;
    resources;
    textures;
    fonts;
    backend;
    mode=State.Game;
    ui;
    win;
    sound;
    random = Random.get_state ();
  }

(* Createa a new state out of the default state for starting the program. *)
let make_state win sound ~region ~reality_levels ~difficulty prev_state =
  let s = prev_state in

  let resources = s.State.resources in
  let textures, fonts = s.State.textures, s.fonts in
  let backend =
    (* Used by different elements *)
    Random.self_init ();
    let random = Random.get_state () in
    let seed = Random.int 0x7FFF random in
    B.make region resources ~random ~seed ~reality_levels ~difficulty
  in
  let map_tex = R.Texture.make win @@ Tilemap.to_img backend.map in
  let map_silhouette_tex = R.Texture.make win @@ Tilemap.to_silhouette backend.map in
  let ui = Main_ui.default win fonts region in
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
    sound;
    random = Random.get_state ();
  }

let handle_tick win (s:State.t) time =
  let state =
    match s.mode with
    | Intro state ->
        let state2, status = Intro.handle_tick time state in
        let s =
          match status with
          | `Stay when state2 === state -> s
          | `Stay -> {s with mode = Intro state2}
          | `Exit -> {s with mode=Menu(Start_menu.default s)}
        in
        s, `Stay

    | Menu _ -> s, `Stay

    | MapGen None ->
        (* Prepare mapgen with init *)
        let cities = B.get_cities s.backend in
        let data = Mapgen.init s.backend.random (B.get_region s.backend) cities in
        {s with mode=MapGen(Some data)}, `Stay

    | MapGen Some data ->
        let done_fn () =
          (* Final update of map *)
          let map = Tilemap.update_heightmap @@ B.get_map s.backend in
          update_map win s map;
          map
        in
        let data, map =
          Iter.(0 -- 60)
          |> Iter.fold (fun (mapgen, map) _ ->
              Mapgen.update_map_step s.backend.random mapgen ~done_fn ~map ~fonts:s.fonts
          )
          (data, B.get_map s.backend)
        in
        let backend = {s.backend with map} in
        {s with backend; mode=MapGen(Some data)}, `Stay

    | Game ->
        (* Main game *)
        (* A tick starts with the backend *)
        let backend, ui_msgs, is_cycle = Backend.handle_tick s.backend time in
        (* Check for need to update map texture *)
        if List.find_opt (function Ui_msg.UpdateMap -> true | _ -> false) ui_msgs |> Option.is_some then
          update_map win s s.backend.map;
        let ui, tick_backend_msgs = Main_ui.handle_tick s s.ui time is_cycle in
        let ui, backend_msgs = Main_ui.handle_msgs s ui ui_msgs in
        let backend_msgs = tick_backend_msgs @ backend_msgs in
        let quit = if List.exists (function Backend.Action.Quit_game -> true | _ -> false) backend_msgs then `Exit else `Stay in
        let backend = Backend.Action.handle_msgs backend backend_msgs in
        [%upf s.ui <- ui];
        [%upf s.backend <- backend];
        s, quit

  in
  state

let handle_event win (s:State.t) (event:Event.t) time =
  (* Handle an input event, starting with the UI.
     (Store the win in closure so we can create the full game state when needed 
   *)
  let state, quit =
    match s.mode with
    | Intro state ->
        begin match Intro.handle_event event state with
        | state2, `Stay when state2 === state -> s, `Stay
        | state2, `Stay -> {s with mode=Intro state2}, `Stay
        | _, `Exit -> {s with mode=Menu(Start_menu.default s)}, `Stay
        end

    | Menu state ->
        begin match Start_menu.handle_event s state event time with
        | state2, `Stay when state2 === state -> s, `Stay
        | state2, `Stay ->
            {s with mode=Menu state2}, `Stay
        | _, `LoadGame s -> s, `Stay
        | _, `Choose (region, difficulty, reality_levels) ->
            let s = make_state win s.sound ~region ~reality_levels ~difficulty s in
            {s with mode=MapGen None}, `Stay
        end

    | MapGen Some {state=`Done; _} ->
        (* Only for map generation *)
        begin match event with
        | Key {down=true; _} -> {s with mode = Game}, `Stay
        | _ -> s, `Stay
        end

    | MapGen _ -> s, `Stay

    | Game ->
        (* Main map view mode *)
        let ui, backend_msgs = Main_ui.handle_event s s.ui event time in
        let backend = Backend.Action.handle_msgs s.backend backend_msgs in
        (* The backend buffers further msgs to ui and sends on next tick *)
        [%upf s.ui <- ui];
        [%upf s.backend <- backend];
        let quit = List.exists (function
          | Backend.Action.Quit_game -> true
          | _ -> false) backend_msgs
        in
        s, if quit then `Exit else `Stay

  in
  state, quit

let render win (s:State.t) = match s.mode with
  | Intro state -> Intro.render win state 

  | Menu state -> Start_menu.render win state s

  | MapGen Some data ->
    let bg_tex = Hashtbl.find s.textures.pics "BRITAIN" in (* generic background *)
    R.clear_screen win;
    R.Texture.render win ~x:0 ~y:0 bg_tex;
    R.Texture.render win ~x:0 ~y:0 s.map_tex;
    Fonts.Render.render s.fonts ~win ~to_render:data.text;
    Mapgen.View.render_new_pixels win data s.textures.pixel

  | MapGen None -> ()

  | Game -> Main_ui.render win s s.ui

let run ?load ~zoom ~adjust_ar () : unit =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);

  Printf.printf "Loading resources...";
  print_newline ();

  let init_fn win =

    let sound = Sound.init () in
    let state = match load with
      | Some slot ->
          Printf.printf "Loading from slot %d...\n" slot;
          Load_game.load_game slot win sound

      | None ->
        (* New game. Use a basic default state *)
        let s = default_state win sound in
        let state = Intro.make s in
        {s with mode=Intro state}
    in
    Printf.printf " done.\n";

    state, Mainloop.{
      handle_event=handle_event win;
      handle_tick=handle_tick win;
      render=render win;
    }
  in
  let zoom = 1 in
  let shader_file = "shaders/test.glsl" in
  Mainloop.main ~zoom ~adjust_ar init_fn ~shader_file

