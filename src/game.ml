open Containers
module R = Renderer
(* open Tsdl *)
module Ndarray = Owl_base_dense_ndarray.Generic

let run ?(view=Screen.MapGen None) ?(area=Gmap.WestUS) () : unit =
  let random = Random.get_state () in
  (* Used by different elements *)
  let seed = Random.int 0x7FFF random in

  Printf.printf "Loading resources...";

  let init_fn win =
    let resources = Resources.load_all ~seed in

    let screen = Screen.make view in

    let map = List.assoc ~eq:(Stdlib.(=)) area resources.res_maps in
    let cities = List.assoc ~eq:(Stdlib.(=)) area resources.res_cities
      |> List.map (fun (name,x,y) -> {Gmap.name;x;y})
      |> Array.of_list in
    let game = {State.map; area; cities} in

    let textures = Textures.of_resources win resources area in
    let ui = Ui.default win in
    let state = {State.game; screen; resources; random; textures; seed; ui} in

    Printf.printf " done.\n";

    let update (s:State.t) (event:Event.t) =
      let state =
        match s.screen.Screen.view with
        | Screen.MapGen None ->
            (* Prepare mapgen with init *)
            let cities = Array.to_list s.game.cities in
            let data = Mapgen.init s.random s.game.area cities in
            Lens.Infix.((State.screen |-- Screen.view) ^= Screen.MapGen(Some data)) s

        | Screen.MapGen Some {state=`Done; _} ->
            begin match event with
            | Key {down=true; _} ->
                    (* Printf.printf "Mapview\n"; *)
                    Lens.Infix.((State.screen |-- Screen.view) ^= Screen.MapView (Mapview.default)) s
            | _ -> s
            end

        | Screen.MapGen Some data ->
            let done_fn () =
              (* Final update of map *)
              Textures.update_map win s.textures s.game.map
            in
            let data =
              Iter.(0 -- 20)
              |> Iter.fold (fun acc _ ->
              Mapgen.update_map_step random acc ~done_fn ~map ~fonts:s.textures.fonts)
              data
            in
            Lens.Infix.((State.screen |-- Screen.view) ^= Screen.MapGen(Some data)) state

        | Screen.MapView data ->
            Mapview.update s data event

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

      | MapView data ->
          Mapview.render win s data
    in
    state, Mainloop.{update; render}
  in
  Mainloop.main init_fn





