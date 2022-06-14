open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer
module B = Backend

let update_map _win v map =
  R.Texture.update v.State.map_tex @@ Tilemap.to_img map

let handle_tick win (s:State.t) (time:int32) =
  let state =
    match s.screen with
    | Screen.MapGen None ->
        (* Prepare mapgen with init *)
        let cities = B.get_cities s.backend in
        let data = Mapgen.init s.backend.random (B.get_region s.backend) cities in
        {s with screen=Screen.MapGen(Some data)}

    | Screen.MapGen Some data ->
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
        {s with backend; screen=Screen.MapGen(Some data)}

    | Screen.MapView ->
        let ui, action = Main_ui.handle_tick s s.ui time in
        s.ui <- ui;
        let backend = 
          Backend.Action.run s.backend action
        in
        s.backend <- backend;
        s

    | _ -> s
  in
  state

let handle_event (s:State.t) (event:Event.t) =
  let state =
    match s.screen with
    | Screen.MapGen Some {state=`Done; _} ->
        begin match event with
        | Key {down=true; _} ->
                (* Printf.printf "Mapview\n"; *)
                {s with screen = Screen.MapView}
        | _ -> s
        end

    | Screen.MapView ->
        let ui, action = Main_ui.handle_event s s.ui event in
        let backend = 
          Backend.Action.run s.backend action
        in
        s.ui <- ui;
        s.backend <- backend;
        s

    | _ -> s
  in
  state, false

let render win (s:State.t) =
  match s.screen with
  | Screen.MapGen Some data ->
      let bg_tex = Hashtbl.find s.textures.pics "BRITAIN" in (* generic background *)
      R.clear_screen win;
      R.Texture.render win ~x:0 ~y:0 bg_tex;
      R.Texture.render win ~x:0 ~y:0 s.map_tex;
      Fonts.Render.render s.fonts ~win ~to_render:data.text;
      Mapgen.View.render_new_pixels win data s.textures.pixel;
      ()
  | MapGen None -> ()

  | MapView ->
      Main_ui.render win s s.ui

  | _ -> ()

let run ?save ?(view=Screen.MapGen None) ?(region=Region.WestUS) () : unit =
  Printf.printf "Loading resources...";

  let init_fn win =
    let random = Random.get_state () in
    (* Used by different elements *)
    let seed = Random.int 0x7FFF random in

    let resources = Resources.load_all () in

    let screen = view in

    let backend = B.default region resources ~random ~seed in

    let textures = Textures.of_resources win resources in

    let map_tex = R.Texture.make win @@ Tilemap.to_img backend.map in

    let fonts = Fonts.load win in

    let ui = Main_ui.default win fonts in

    let state = {
      map_tex;
      State.screen;
      backend;
      resources;
      textures;
      fonts;
      ui;
    }
    in

    Printf.printf " done.\n";

    state, Mainloop.{
      handle_event;
      handle_tick=handle_tick win;
      render=render win;
    }
  in
  Mainloop.main init_fn





