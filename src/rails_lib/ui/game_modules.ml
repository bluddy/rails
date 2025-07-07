open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer
module B = Backend
open Utils.Infix

let update_map _win v map =
  R.Texture.update v.State.map_tex @@ Tilemap.to_img map

let handle_tick win (s:State.t) time =
  let state =
    match s.mode with
    | MapGen None ->
        (* Prepare mapgen with init *)
        let cities = B.get_cities s.backend in
        let data = Mapgen.init s.backend.random (B.get_region s.backend) cities in
        {s with mode=MapGen(Some data)}

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
        {s with backend; mode=MapGen(Some data)}

    | Game ->
        (* Main game *)
        (* A tick starts with the backend *)
        let backend, ui_msgs, is_cycle = Backend.handle_tick s.backend time in
        if List.find_opt (function Ui_msg.UpdateMap -> true | _ -> false) ui_msgs |> Option.is_some then
          update_map win s s.backend.map;
        let ui = Main_ui.handle_tick s s.ui time is_cycle in
        let ui, backend_msgs = Main_ui.handle_msgs s ui ui_msgs in
        let backend = Backend.Action.handle_msgs backend backend_msgs in
        [%upf s.ui <- ui];
        [%upf s.backend <- backend];
        s

    | Intro state ->
        let state2 = Intro.handle_tick time state in
        if state2 === state then s
        else {s with mode=Intro state2}
  in
  state

let handle_event (s:State.t) (event:Event.t) =
  (* Handle an input event, starting with the UI *)
  let state, quit =
    match s.mode with
    | Intro state ->
        begin match Intro.handle_event event state with
        | `None, state2 when state2 === state -> s, false
        | `None, state2 -> {s with mode=Intro state2}, false
        | `Exit, _ -> {s with mode=MapGen None}, false
        end

    | MapGen Some {state=`Done; _} ->
        (* Only for map generation *)
        begin match event with
        | Key {down=true; _} -> {s with mode = Game}, false
        | _ -> s, false
        end

    | MapGen _ -> s, false

    | Game ->
        (* Main map view mode *)
        let ui, backend_msgs = Main_ui.handle_event s s.ui event in
        let backend = Backend.Action.handle_msgs s.backend backend_msgs in
        (* The backend buffers further msgs to ui and sends on next tick *)
        [%upf s.ui <- ui];
        [%upf s.backend <- backend];
        let quit = List.exists (function
          | Backend.Action.Quit_game -> true
          | _ -> false) backend_msgs
        in
        s, quit
  in
  state, quit

let render win (s:State.t) = match s.mode with
  | MapGen Some data ->
    let bg_tex = Hashtbl.find s.textures.pics "BRITAIN" in (* generic background *)
    R.clear_screen win;
    R.Texture.render win ~x:0 ~y:0 bg_tex;
    R.Texture.render win ~x:0 ~y:0 s.map_tex;
    Fonts.Render.render s.fonts ~win ~to_render:data.text;
    Mapgen.View.render_new_pixels win data s.textures.pixel;
    ()
  | MapGen None -> ()

  | Game -> Main_ui.render win s s.ui

  | Intro state -> Intro.render win s state

let run ?load ?(region=Region.WestUS) () : unit =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Debug);

  Printf.printf "Loading resources...";
  print_newline ();

  let init_fn win =

    let create_state ?backend ?ui_options ?ui_view mode =
        let resources = Resources.load_all () in

        let backend = match backend with
          | Some b -> b
          | None ->
            (* Used by different elements *)
            Random.self_init ();
            let random = Random.get_state () in
            let seed = Random.int 0x7FFF random in
            B.default region resources ~random ~seed
        in
        let textures = Textures.of_resources win resources in
        let map_tex = R.Texture.make win @@ Tilemap.to_img backend.map in
        let map_silhouette_tex = R.Texture.make win @@ Tilemap.to_silhouette backend.map in
        let fonts = Fonts.load win in
        let ui = Main_ui.default ?options:ui_options ?view:ui_view win fonts region in
        {
          map_tex;
          map_silhouette_tex;
          State.mode;
          backend;
          resources;
          textures;
          fonts;
          ui;
        }
    in
    let state = match load with
      | Some savefile ->
          Printf.printf "Loading %s...\n" savefile;
          let s = IO.File.read_exn savefile in
          let lst = String.split s ~by:"====" in
          Printf.printf "len[%d]\n%!" (List.length lst);
          begin match lst with
          | [backend;options;view] ->
              let backend = Yojson.Safe.from_string backend |> Backend.t_of_yojson in
              let backend = {backend with pause = false} in
              Backend.reset_tick backend;
              let ui_options = Yojson.Safe.from_string options |> Main_ui_d.options_of_yojson in
              let ui_view = Yojson.Safe.from_string view |> Mapview_d.t_of_yojson in
              create_state ~backend ~ui_options ~ui_view Game
          | _ -> assert false
          end

      | None ->
        (* New game. Just use a default *)
        let s = create_state Game in
        let state = Intro.create s in
        {s with mode=Intro state}
    in
    Printf.printf " done.\n";

    state, Mainloop.{
      handle_event;
      handle_tick=handle_tick win;
      render=render win;
    }
  in
  Mainloop.main init_fn

