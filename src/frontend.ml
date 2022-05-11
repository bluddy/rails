open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer
module B = Backend

let run ?(view=Screen.MapGen None) ?(area=Gmap.WestUS) () : unit =
  let random = Random.get_state () in
  (* Used by different elements *)
  let seed = Random.int 0x7FFF random in

  Printf.printf "Loading resources...";

  let init_fn win =
    let resources = Resources.load_all ~seed in

    let screen = Screen.make view in

    let backend = B.default area resources in

    let textures = Textures.of_resources win resources area in

    let ui = Main_ui.default win textures.fonts in

    let state = {
      State.screen;
      backend;
      resources;
      random;
      textures;
      seed;
      ui;
    } in

    Printf.printf " done.\n";

    let update (s:State.t) (event:Event.t) =
      let state =
        match s.screen.Screen.view with
        | Screen.MapGen None ->
            (* Prepare mapgen with init *)
            let cities = Array.to_list @@ B.get_cities s.backend in
            let data = Mapgen.init s.random (B.get_area s.backend) cities in
            Lens.Infix.((State.screen |-- Screen.view) ^= Screen.MapGen(Some data)) s

        | Screen.MapGen Some {state=`Done; _} ->
            begin match event with
            | Key {down=true; _} ->
                    (* Printf.printf "Mapview\n"; *)
                    Lens.Infix.((State.screen |-- Screen.view) ^= Screen.MapView) s
            | _ -> s
            end

        | Screen.MapGen Some data ->
            let done_fn () =
              (* Final update of map *)
              Textures.update_map win s.textures @@ B.get_map s.backend
            in
            let data =
              Iter.(0 -- 20)
              |> Iter.fold (fun acc _ ->
                  let map = B.get_map s.backend in
                  Mapgen.update_map_step random acc ~done_fn ~map ~fonts:s.textures.fonts)
              data
            in
            Lens.Infix.((State.screen |-- Screen.view) ^= Screen.MapGen(Some data)) state

        | Screen.MapView ->
            let ui, actions = Main_ui.update s s.ui event in
            s.ui <- ui;
            let backend = 
              Backend.Action.run_many s.backend actions
            in
            s.backend <- backend;
            s

        | _ -> s
      in
      state, false
    in
    let render (s:State.t) =
      match s.screen.Screen.view with
      | Screen.MapGen Some data ->
          let bg_tex = Hashtbl.find s.textures.pics "BRITAIN" in (* generic background *)
          R.clear_screen win;
          R.Texture.render win ~x:0 ~y:0 bg_tex;
          R.Texture.render win ~x:0 ~y:0 s.textures.map;
          Fonts.Render.render s.textures.fonts ~win ~to_render:data.text;
          Mapgen.View.render_new_pixels win data s.textures.pixel;
          s
      | MapGen None -> s

      | MapView ->
          Main_ui.render win s s.ui

      | _ -> s
    in
    state, Mainloop.{update; render}
  in
  Mainloop.main init_fn





