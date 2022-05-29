open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer

module TileTex = struct

  type t =
    | Single of R.Texture.t
    | Pair of R.Texture.t * R.Texture.t
    | Localized of {us: t; england: t; europe: t}

  (* Load and separate sprites *)
  let slice_tiles win res =
    let open Tilemap in
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
    let extra_ndarray = Hashtbl.find res.res_pics "SPRITES_extra" in 

    let load_us_tiles ~tiles ~y ~mult =
      let pair = pair_fn ~tiles ~ndarray:us_ndarray ~y ~mult in
      let top = single_fn ~tiles ~ndarray:us_ndarray ~y ~mult in
      let bottom = single_fn ~tiles ~ndarray:us_ndarray ~y:(y+mult) ~mult in
      pair Tile.Clear 0;
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
      (* bottom EnemyRR 13; *)
      bottom TextileMill 14;
    in
    load_us_tiles ~tiles ~y:0 ~mult:16;
    load_us_tiles ~tiles:small_tiles ~y:32 ~mult:8;

    let load_en_tiles ~tiles ~y ~mult =
      let top = single_fn ~tiles ~ndarray:en_ndarray ~y ~mult in
      let bottom = single_fn ~tiles ~ndarray:en_ndarray ~y:(y+mult) ~mult in
      top Tile.Brewery 10;
      top SheepFarm 11;
      top GlassWorks 12;
      bottom SaltMine 11;
    in
    load_en_tiles ~tiles ~y:0 ~mult:16;
    load_en_tiles ~tiles:small_tiles ~y:32 ~mult:8;

    let load_eu_tiles ~tiles ~y ~mult =
      let top = single_fn ~tiles ~ndarray:eu_ndarray ~y ~mult in
      top Tile.Winery 10;
      top Fort 11;
      top Vineyard 13;
    in
    load_eu_tiles ~tiles ~y:0 ~mult:16;
    load_eu_tiles ~tiles:small_tiles ~y:32 ~mult:8;

    (* We added these *)
    let load_extra_tiles ~tiles ~y ~mult =
      let pair = pair_fn ~tiles ~ndarray:extra_ndarray ~y ~mult in
      pair Tile.Desert 2;
    in
    load_extra_tiles ~tiles ~y:0 ~mult:16;
    load_extra_tiles ~tiles:small_tiles ~y:32 ~mult:8;

    (* Load cities and villages, which are slightly different per region *)
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

    let load_dir_tiles ~tiles ~key ~y ~x ~mult ~ndarray =
      let img i dirs =
        let x1, x2 = x + i * mult, x + (i+1) * mult - 1 in
        let y1, y2 = y, y + mult - 1 in
        let slice =
          Ndarray.get_slice [[y1; y2]; [x1; x2]] ndarray
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
    let ocean x = Tile.Ocean x in
    let harbor x = Tile.Harbor x in
    let river x = Tile.River x in
    let landing x = Tile.Landing x in
    load_dir_tiles ~tiles ~key:ocean ~y:48 ~x:0 ~mult:16 ~ndarray:us_ndarray;
    load_dir_tiles ~tiles ~key:harbor ~y:48 ~x:0 ~mult:16 ~ndarray:extra_ndarray;
    load_dir_tiles ~tiles ~key:river ~y:64 ~x:0 ~mult:16 ~ndarray:us_ndarray;
    load_dir_tiles ~tiles ~key:landing ~y:64 ~x:0 ~mult:16 ~ndarray:extra_ndarray;
    load_dir_tiles ~tiles:small_tiles ~key:ocean ~y:32 ~x:160 ~mult:8 ~ndarray:us_ndarray;
    load_dir_tiles ~tiles:small_tiles ~key:harbor ~y:32 ~x:160 ~mult:8 ~ndarray:extra_ndarray;
    load_dir_tiles ~tiles:small_tiles ~key:river ~y:40 ~x:160 ~mult:8 ~ndarray:us_ndarray;
    load_dir_tiles ~tiles:small_tiles ~key:landing ~y:40 ~x:160 ~mult:8 ~ndarray:extra_ndarray;
    
    (tiles, small_tiles)

    let find hash tile ~region ~alt =
      let v = 
        try
          Hashtbl.find hash tile
        with Not_found ->
          failwith @@ Printf.sprintf "Tile %s not found" (Tile.show tile)
      in
      let rec find = function
        | Single x -> x
        | Pair(_, x) when alt -> x
        | Pair(x, _) -> x
        | Localized x ->
            match region with
            | Region.WestUS | EastUS -> find x.us
            | Britain -> find x.england
            | Europe -> find x.europe
      in
      find v
end

module Tracks = struct
  let tile_w, tile_h = 20, 20

  let load win res =
    let ndarray = Hashtbl.find res.Resources.res_pics "TRACKS" in
    let extra_ndarray = Hashtbl.find res.res_pics "TRACKS_extra" in
    let w = (Ndarray.shape ndarray).(1) in
    let num_j = w / tile_w in
    let track_dict = Track.Htbl.create 100 in

    let get_tex ?(extra=false) i j =
      let x, y = j * tile_w, i * tile_h in
      let ndarr = if extra then extra_ndarray else ndarray in
      let slice = Ndarray.get_slice [[y; y + tile_h - 1]; [x; x + tile_w - 1]] ndarr in
      let tex = R.Texture.make win slice in
      tex
    in
    let load_textures ?extra start_i start_j kind dirslist =
      List.fold_left (fun (i,j) li ->
        let tex = get_tex ?extra i j in
        let dirs = Dir.Set.of_list li in
        let track = Track.make dirs kind ~player:0 in
        Track.Htbl.replace track_dict track tex;
        if j >= num_j - 1 then
          (i + 1, 0)
        else
          (i, j + 1)
      )
      (start_i, start_j)
      dirslist
      |> ignore
    in

    (* load regular tracks *)
    let all_tracks = Resources.(track_dirs @ illegal_track @ track_turns) in 
    let open Track in
    load_textures 0 0 Track all_tracks;
    load_textures ~extra:true 0 0 Ferry all_tracks;

    load_textures 3 0 Tunnel Resources.special_dirs;
    load_textures 3 4 (Bridge(Bridge.Iron)) Resources.special_dirs;
    load_textures 3 8 (Bridge(Bridge.Wood)) Resources.special_dirs;
    load_textures ~extra:true 3 8 (Bridge(Bridge.Stone)) Resources.special_dirs;
    load_textures 4 0 (Station(`SignalTower)) Resources.special_dirs;
    load_textures 4 0 (Station(`Depot)) Resources.special_dirs;
    load_textures 4 4 (Station(`Station)) Resources.special_dirs;
    load_textures 4 8 (Station(`Terminal)) Resources.special_dirs;
    track_dict

  let find track_h track =
    Track.Htbl.find track_h track

end

module Station = struct
    let ndarray = Hashtbl.find res.Resources.res_pics "STATION" in
    let tex x y x2 y2 =
      Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] ndarray
      |> R.Texture.make win slice
    in
    let bgnd = tex 0 141 320 200 in
    let depot = tex 6 10 102 42 in
    let station = tex 112 27 209 83 in
    let terminal = tex 216 13 320 123 in
    let switching_yard = tex 105 2 215 18 in
    let engine_shop = tex 1 114 50 138 in
    let barn = tex 2 88 39 113 in
    let fence = tex 76 120 111 129 in
    let goods_bottom = tex 76 108 111 116 in
    let goods = tex 76 90 111 104 in
    let cold = tex 76 60 111 80 in
    let smokestacks = tex 76 49 111 56 in
    let hotel = tex 112 87 147 140 in
    let restaurant = tex 148 111 191 128 in
    let rest_bottom = tex 148 132 191 140 in
    let post_office = tex 148 94 191 107 in
    let post_top = tex 148 87 191 90 in
    ()

end

module Cars = struct
  let tile_w, tile_h = 20, 20

  let load win res =
    let ndarray = Hashtbl.find res.Resources.res_pics "TRACKS" in
    let width = (Ndarray.shape ndarray).(1) in
    let car_dict = Hashtbl.create 100 in

    let get_tex x y =
      Ndarray.get_slice [[y; y + tile_h - 1]; [x; x + tile_w - 1]] ndarray
      |> R.Texture.make win
    in
    let tex x y kind =
      List.fold_left (fun x dir ->
        let tex = get_tex x y in
        Hashtbl.replace car_dict (kind,dir) tex;
        (x + tile_w) mod width
      )
      x
      Dir.dirlist
      |> ignore
    in

    tex 200 120 `BigOldEngine;
    tex 200 140 `LittleOldEngine;
    tex 40 120 `DieselEngine;
    tex 40 140 `MailCar;
    tex 200 160 `PaseengerCar;
    tex 40 160 `FastCar;
    tex 200 180 `SlowCar;
    tex 40 180 `BulkCar;

end

module Smoke = struct
    let ndarray = Hashtbl.find res.Resources.res_pics "TRACKS"
    let tex x y =
      Ndarray.get_slice [[y; y + 20 - 1]; [x; x + 20 - 1]] ndarray
      |> R.Texture.make win slice
    let smoke1 = tex 220 100
    let smoke2 = tex 240 100
    let smoke3 = tex 260 100
    let smoke4 = tex 280 100
end

module Cursor = struct
    let ndarray = Hashtbl.find res.Resources.res_pics "TRACKS"
    let tex x y =
      Ndarray.get_slice [[y; y + 20 - 1]; [x; x + 20 - 1]] ndarray
      |> R.Texture.make win slice
    let tex = tex 100 300
end

let slice_logo win res =
  let ndarray = Hashtbl.find res.Resources.res_pics "SPRITES" in
  Ndarray.get_slice [[63; 118]; [256; 319]] ndarray
  |> R.Texture.make win

type t = {
  maps: (Region.t * R.Texture.t) list;
  pics: (string, R.Texture.t) Hashtbl.t;
  mutable map: R.Texture.t;   (* current map *)
  pixel: R.Texture.t; (* white pixel *)
  fonts: Fonts.t;
  logo: R.Texture.t;
  tiles: (Tile.t, TileTex.t) Hashtbl.t;
  small_tiles: (Tile.t, TileTex.t) Hashtbl.t;
  tracks: R.Texture.t Track.Htbl.t;
}

let of_resources win res region =
  let maps = List.map (fun (a, v) ->
      a, R.Texture.make win @@ Tilemap.to_img v)
    res.Resources.res_maps
  in
  let map = List.assoc ~eq:(Stdlib.(=)) region maps in
  let pics = Hashtbl.to_iter res.res_pics
    |> Iter.map (fun (s, arr) -> s, R.Texture.make win arr)
    |> Hashtbl.of_iter
  in
  let pixel = R.Texture.make win Pic.white_pixel in
  let fonts = Fonts.load win in
  let tiles, small_tiles = TileTex.slice_tiles win res in
  let logo = slice_logo win res in
  let tracks = Tracks.load win res in
  {
    maps;
    pics;
    map;
    pixel;
    fonts;
    tiles;
    small_tiles;
    logo;
    tracks;
  }

let update_map _win v map =
  R.Texture.update v.map @@ Tilemap.to_img map

