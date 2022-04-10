open Containers
module R = Renderer
(* open Tsdl *)
module Ndarray = Owl_base_dense_ndarray.Generic


(* The actual game (server) state *)
type game = {
  area: Gmap.area;
  map : Gmap.t;
  cities: Gmap.city array;
}
[@@deriving lens]


module Textures = struct
  module R = Renderer
  type t = {
    maps: (Gmap.area * R.Texture.t) list;
    pics: (string, R.Texture.t) Hashtbl.t;
    map: R.Texture.t;
    pixel: R.Texture.t;
    fonts: Fonts.t;
  }

  let of_resources win res area =
    let maps = List.map (fun (a, v) ->
        a, R.Texture.make win @@ Gmap.to_img v)
      res.Resources.res_maps
    in
    let map = List.assoc ~eq:(Stdlib.(=)) area maps in
    let pics = Hashtbl.to_iter res.res_pics
      |> Iter.map (fun (s, arr) -> s, R.Texture.make win arr)
      |> Hashtbl.of_iter
    in
    let pixel = R.Texture.make win Pic.white_pixel in
    let fonts = Fonts.load win in
    {maps; pics; map; pixel; fonts}
end

type state = {
  random: Random.State.t;
  random_seed: int;
  game: game;
  screen: Screen.t;
  resources: Resources.t;
  textures: Textures.t;
}
[@@deriving lens]

let run ?(view=Screen.MapGen None) ?(area=Gmap.WestUS) () : unit =
  let random = Random.get_state () in
  (* Used by different elements *)
  let random_seed = Random.int 0x7FFF random in

  Printf.printf "Loading resources...";

  let init_fn win =
    let resources = Resources.load_all ~random_seed in

    let screen = Screen.make view in

    let map = List.assoc ~eq:(Stdlib.(=)) area resources.res_maps in
    let cities = List.assoc ~eq:(Stdlib.(=)) area resources.res_cities
      |> Array.of_list in
    let game = {map; area; cities} in

    let textures = Textures.of_resources win resources area in
    let state = {game; screen; resources; random; textures; random_seed} in

    Printf.printf " done.\n";

    let update (s:state) _event =
      let state =
        match s.screen.Screen.view with
        | Screen.MapGen None ->
            (* Prepare mapgen with init *)
            let cities = List.assoc ~eq:(Gmap.equal_area) area s.resources.res_cities in
            let data = Mapgen.init s.random s.game.area cities in
            Lens.Infix.((state_screen |-- Screen.view) ^= Screen.MapGen(Some data)) state
        | Screen.MapGen Some data ->
            let data =
              Iter.(0 -- 6)
              |> Iter.fold (fun acc _ ->
              Mapgen.update_map_step random acc ~map ~fonts:s.textures.fonts)
              data
            in
            Lens.Infix.((state_screen |-- Screen.view) ^= Screen.MapGen(Some data)) state

        | _ -> s
      in
      state, false
    in
    let render (s:state) =
      match s.screen.Screen.view with
      | Screen.MapGen Some data ->
          let open Result.Infix in
          let bg_tex = Hashtbl.find s.textures.pics "BRITAIN" in (* generic background *)
          let () = R.error_handle @@
            let* () = R.clear_screen win in
            let* () = R.render win bg_tex in
            let* () = R.render win @@ List.assoc ~eq:(Gmap.equal_area) area s.textures.maps in
            let* () = Fonts.Render.render s.textures.fonts ~win ~to_render:data.text in
            let* () = Mapgen.render_new_pixels win data s.textures.pixel in
            Result.return ()
          in
          s

      | Screen.MapGen None -> s
    in
    state, Graphics.{update; render}
  in
  Graphics.main init_fn





