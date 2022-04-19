open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer

module Tile = struct

  type t =
    | Single of R.Texture.t
    | Pair of R.Texture.t * R.Texture.t
    | Localized of {us: t; england: t; europe: t}

  (* Load and separate sprites *)
  let slice_tiles win res =
    let open Gmap in
    let tiles = Hashtbl.create 40 in
    let small_tiles = Hashtbl.create 40 in

    let pair_part ~ndarray ~y ~mult i =
      let x1, x2 = i * mult, (i+1) * mult - 1 in
      let y1, y2 = y, y + mult - 1 in
      let p1 = Ndarray.get_slice [[y1; y2]; [x1; x2]] ndarray in
      let y1, y2 = y + mult, y + 2 * mult - 1 in
      let p2 = Ndarray.get_slice [[y1; y2]; [x1; x2]] ndarray in
      Pair(R.Texture.make win p1, R.Texture.make win p2)
    in
    let pair_fn ~tiles ~ndarray ~y ~mult key i =
      let p = pair_part ~ndarray ~y ~mult i in
      Hashtbl.replace tiles key p
    in
    let single_fn ~tiles ~ndarray ~y ~mult key i =
      let x1, x2 = i * mult, (i+1) * mult - 1 in
      let y1, y2 = y, y + mult - 1 in
      (* Printf.printf "x[%d-%d] y[%d-%d]\n" x1 x2 y (y + mult); *)
      let p1 =
        Ndarray.get_slice [[y1; y2]; [x1; x2]] ndarray |> R.Texture.make win
      in
      Hashtbl.replace tiles key @@ Single p1
    in

    let us_ndarray = Hashtbl.find res.Resources.res_pics "SPRITES" in
    let en_ndarray = Hashtbl.find res.res_pics "ESPRITES" in
    let eu_ndarray = Hashtbl.find res.res_pics "CSPRITES" in

    single_fn ~tiles ~ndarray:us_ndarray ~y:40 ~mult:16 Harbor 17;

    let load_us_tiles ~tiles ~y ~mult =
      let pair = pair_fn ~tiles ~ndarray:us_ndarray ~y ~mult in
      let top = single_fn ~tiles ~ndarray:us_ndarray ~y ~mult in
      let bottom = single_fn ~tiles ~ndarray:us_ndarray ~y:(y+mult) ~mult in
      pair Clear 0;
      pair Woods 1;
      pair Swamp 2;
      pair Desert 2; (* Needs to be yellow *)
      pair Foothills 3;
      pair Hills 4;
      pair Mountains 5;
      (* pair City 6; *)
      (* pair Village 7; *)
      pair Farm 8;
      pair Slums 9;
      top FoodProc 10;
      top Ranch 11;
      top Stockyard 12;
      top Factory 13;
      top GrainElev 14;
      top PaperMill 15;
      top LumberMill 17;
      top CoalMine 18;
      top SteelMill 19;
      bottom PowerPlant 10;
      bottom OilWell 11;
      bottom Refinery 12;
      bottom EnemyRR 13;
      bottom TextileMill 14;
    in
    load_us_tiles ~tiles ~y:0 ~mult:16;
    load_us_tiles ~tiles:small_tiles ~y:32 ~mult:8;

    let load_en_tiles ~tiles ~y ~mult =
      let top = single_fn ~tiles ~ndarray:en_ndarray ~y ~mult in
      let bottom = single_fn ~tiles ~ndarray:en_ndarray ~y:(y+mult) ~mult in
      top Brewery 10;
      top SheepFarm 11;
      top GlassWorks 12;
      bottom SaltMine 11;
    in
    load_en_tiles ~tiles ~y:0 ~mult:16;
    load_en_tiles ~tiles:small_tiles ~y:32 ~mult:8;

    let load_eu_tiles ~tiles ~y ~mult =
      let top = single_fn ~tiles ~ndarray:eu_ndarray ~y ~mult in
      top Winery 10;
      top Fort 11;
      top Vinyard 13;
    in
    load_eu_tiles ~tiles ~y:0 ~mult:16;
    load_eu_tiles ~tiles:small_tiles ~y:32 ~mult:8;

    (* Load cities and villages, which are slightly different per area *)
    let load_localized ~tiles ~y ~mult key i =
      let us = pair_part ~ndarray:us_ndarray ~y ~mult i in
      let england = pair_part ~ndarray:en_ndarray ~y ~mult i in
      let europe = pair_part ~ndarray:eu_ndarray ~y ~mult i in
      Hashtbl.replace tiles key @@ Localized{us;england;europe}
    in
    load_localized ~tiles ~y:0 ~mult:16 City 6;
    load_localized ~tiles ~y:0 ~mult:16 Village 7;
    load_localized ~tiles:small_tiles ~y:32 ~mult:8 City 6;
    load_localized ~tiles:small_tiles ~y:32 ~mult:8 Village 7;

    let load_dir_tiles ~tiles ~key ~y ~x ~mult =
      let img i dirs =
        let x1, x2 = x + i * mult, x + (i+1) * mult - 1 in
        let y1, y2 = y, y + mult - 1 in
        let slice =
          Ndarray.get_slice [[y1; y2]; [x1; x2]] us_ndarray
          |> R.Texture.make win
        in
        Hashtbl.replace tiles (key @@ Dir.Set.of_list dirs) @@ Single slice
      in
      let open Dir in
      img 0 [];
      img 1 [Up];
      img 2 [Right];
      img 3 [Up; Right];
      img 4 [Down];
      img 5 [Up; Down];
      img 6 [Down; Right];
      img 7 [Up; Down; Right];
      img 8 [Left];
      img 9 [Up; Left];
      img 10 [Left; Right];
      img 11 [Left; Right; Up];
      img 12 [Left; Down];
      img 13 [Up; Left; Down];
      img 14 [Left; Down; Right];
      img 15 [Up; Right; Left; Down]
    in
    let ocean x = Ocean x in
    let river x = River x in
    let landing x = Landing x in
    load_dir_tiles ~tiles ~key:ocean ~y:48 ~x:0 ~mult:16;
    load_dir_tiles ~tiles ~key:river ~y:64 ~x:0 ~mult:16;
    (* TODO: make it bright *)
    load_dir_tiles ~tiles ~key:landing ~y:64 ~x:0 ~mult:16;
    load_dir_tiles ~tiles:small_tiles ~key:ocean ~y:32 ~x:160 ~mult:8;
    load_dir_tiles ~tiles:small_tiles ~key:river ~y:40 ~x:160 ~mult:8;
    load_dir_tiles ~tiles:small_tiles ~key:landing ~y:40 ~x:160 ~mult:8;
    
    (tiles, small_tiles)

    let find hash tile ~area ~alt =
      let v = 
        try
          Hashtbl.find hash tile
        with Not_found ->
          failwith @@ Printf.sprintf "Tile %s not found" (Gmap.show_tile tile)
      in
      let rec find = function
        | Single x -> x
        | Pair(_, x) when alt -> x
        | Pair(x, _) -> x
        | Localized x ->
            match area with
            | Gmap.WestUS | EastUS -> find x.us
            | Britain -> find x.england
            | Europe -> find x.europe
      in
      find v
end

type t = {
  maps: (Gmap.area * R.Texture.t) list;
  pics: (string, R.Texture.t) Hashtbl.t;
  mutable map: R.Texture.t;   (* current map *)
  pixel: R.Texture.t; (* white pixel *)
  fonts: Fonts.t;
  tiles: (Gmap.tile, Tile.t) Hashtbl.t;
  small_tiles: (Gmap.tile, Tile.t) Hashtbl.t;
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
  let tiles, small_tiles = Tile.slice_tiles win res in
  {maps; pics; map; pixel; fonts; tiles; small_tiles}

let update_map win v map =
  (* R.Texture.destroy v.map; *)
  (* let tex = R.Texture.make win @@ Gmap.to_img map in *)
  (* v.map <- tex *)
  R.Texture.update v.map @@ Gmap.to_img map

