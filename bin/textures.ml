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
  let load win res =
    let ndarray = Hashtbl.find res.Resources.res_pics "STATION" in
    let hash = Hashtbl.create 20 in
    let tex key x y x2 y2 =
      let tex = 
        Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] ndarray |> R.Texture.make win
      in
      Hashtbl.replace hash key tex
    in
    tex `Background 0 141 320 200;
    tex `Depot 6 10 102 42;
    tex `Station 112 27 209 83;
    tex `Terminal 216 13 320 123;
    tex `SwitchingYard 105 2 215 18;
    tex `EngineShop 1 114 50 138;
    tex `Barn 2 88 39 113;
    tex `Fence 76 120 111 129;
    tex `Goods_bottom 76 108 111 116;
    tex `Goods 76 90 111 104;
    tex `Cold 76 60 111 80;
    tex `Smokestacks 76 49 111 56;
    tex `Hotel 112 87 147 140;
    tex `Restaurant 148 111 191 128;
    tex `Rest_bottom 148 132 191 140;
    tex `PostOffice 148 94 191 107;
    tex `Post_top 148 87 191 90;
    hash

end

module CarsTop = struct
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

module CarSide = struct
  let load win res =
    let ndarray = Hashtbl.find res.Resources.res_pics "SPRITES" in
    let hash = Hashtbl.create 10 in

    let tex_full ~dx ~dy key x y =
      let tex = Ndarray.get_slice [[y; y + dy - 1]; [x; x + dx - 1]] ndarray |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    let tex = tex_full ~dx:63 ~dy:9 in
    tex `Engine1 63 96;  (* transparent background *)
    tex `Engine2 63 106;
    tex `Engine3 63 116;
    tex `Engine4 63 126;
    tex `Engine5 63 136;
    tex `Engine6 63 146;
    tex `Engine7 63 156;
    tex `Engine8 63 166;
    tex `Engine9 63 176;
    tex `EngineW1 0 96;  (* white background *)
    tex `EngineW2 0 106;
    tex `EngineW3 0 116;
    tex `EngineW4 0 126;
    tex `EngineW5 0 136;
    tex `EngineW6 0 146;
    tex `EngineW7 0 156;
    tex `EngineW8 0 166;
    tex `EngineW9 0 176;

    let tex = tex_full ~dx:20 ~dy:8 in
    tex `CarMailEmpty 139 97;
    tex `CarMail 160 97;
    tex `CarMail2 180 97;
    tex `Caboose 200 97;
    tex `CarPassengerEmpty 139 107;
    tex `CarPassenger 139 107;
    tex `CarPassengerGreen 160 107;
    tex `CarPassenger2 180 107;
    tex `CarFastEmpty 139 117;
    tex `CarFood 160 117;
    tex `CarLivestock 180 117;
    tex `CarGoods 200 117;
    tex `CarSlowEmpty 139 127;
    tex `CarGrain 160 127;
    tex `CarPaper 180 127;
    tex `CarSteel 200 127;
    tex `CarBulkEmpty 139 137;
    tex `CarPetroleum 160 137;
    tex `CarWood 180 137;
    tex `CarCoal 200 137;
    let tex = tex_full ~dx:24 ~dy:8 in
    tex `CarBigMail 160 147;
    tex `CarBigPassenger 160 157;
    tex `CarBigPassengerGreen 184 157;
    tex `CarBigPassengerGreenBig 208 157;
    tex `CarBigFood 160 167;
    tex `CarBigLivestock 184 167;
    tex `CarBigGoods 208 167;
    tex `CarBigGrain 160 177;
    tex `CarBigPaper 184 177;
    tex `CarBigSteel 208 177;
    tex `CarBigPetroleum 160 187;
    tex `CarBigWood 184 187;
    tex `CarBigCoal 208 187;
    hash

end

module TrainAnim = struct

  type t = {
    tex: R.Texture.t;
    w: int;
    h: int;
    anim_x: int;
    anim_y: int;
    anim: R.Texture.t array;
    smoke_x: int option;
  }

  let load win res =
    let hash = Hashtbl.create 10 in
    let ndarray = Hashtbl.find res.Resources.res_pics "LOCOS" in

    let part ?(arr=ndarray) x y x2 y2 =
      Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] arr |> R.Texture.make win
    in
    let engine kind ?(anim_x=0) ?(anim_y=0) ?smoke_x ?(anim=[||]) x y x2 =
      let y2 = y + 23 in
      let tex = part x y x2 y2 in
      let w, h = x2 - x, y2 - y in
      let engine = {
        tex; w; h; anim_x; anim_y; anim; smoke_x
      }
      in
      Hashtbl.add hash kind engine
    in

    engine `Engine1 0 5 32 ~anim_x:10 ~anim_y:0
      ~anim: [|
        part 140 5 159 24;
        part 140 29 159 48;
      |];
    engine `Engine2 0 29 49 ~anim_x:23 ~anim_y:12
      ~anim: [|
        part 109 177 133 184;
        part 109 185 133 192;
        part 135 177 159 184;
        part 135 185 159 192;
      |];
    engine `Engine3 0 53 74 ~anim_x:31 ~anim_y:10
      ~anim: [|
        part 90 77 123 86;
        part 90 87 123 96;
        part 125 77 158 86;
        part 125 87 158 96;
      |];
    engine `Engine4 0 76 86 ~anim_x:38 ~anim_y:13
      ~anim: [|
        part 34  9 73 16;
        part 34 17 73 24;
        part 75  9 114 16;
        part 75 17 114 24;
      |];
    engine `Engine5 0 98 85 ~anim_x:44 ~anim_y:15
      ~anim: [|
        part 86 104 119 111;
        part 86 113 119 120;
        part 126 104 159 111;
        part 126 113 159 120;
      |];
    engine `Engine6 0 123 89 ~anim_x:44 ~anim_y:136
      ~anim: [|
        part 80 55 118 63;
        part 80 64 118 73;
        part 120 55 158 63;
        part 120 64 158 73;
      |];
    engine `Engine7 0 149 116 ~anim_x:70 ~anim_y:10
      ~anim: [|
        part 53 29 95 38;
        part 53 39 95 48;
        part 96 29 138 38;
        part 96 39 138 48;
      |];
    engine `Engine8 0 175 106 ~anim_x:66 ~anim_y:10
      ~anim: [|
        part 90 129 124 136;
        part 90 137 124 144;
        part 125 129 159 136;
        part 125 137 159 144;
      |];
    engine `Engine9 160 2 319;
    engine `Engine10 160 27 319;

    let car ?(h=19) ?(w=47) ?arr kind x y =
      let y2 = y + h in
      let x2 = x + w in
      let tex = part ?arr x y x2 y2 in
      let car = {
        tex; w; h; anim_x=0; anim_y=0; anim=[||]; smoke_x=None;
      }
      in
      Hashtbl.add hash kind car
    in
    car `CarMail 160 80;
    car `CarFood 160 100;
    car `CarGoods 160 120;
    car `CarPaper 160 140;
    car `CarPetroleum 160 160;
    car `CarCoal 160 180;
    car `CarPassenger 240 80;
    car `CarLivestock 240 100;
    car `CarGrain 240 120;
    car `CarSteel 240 140;
    car `CarWood 240 160;

    let ndarray = Hashtbl.find res.Resources.res_pics "LOCOSM" in
    let car = car ~w:60 ~arr:ndarray in

    car `CarBigMail 160 80;
    car `CarBigFood 160 100;
    car `CarBigGoods 160 120;
    car `CarBigPaper 160 140;
    car `CarBigPetroleum 160 160;
    car `CarBigCoal 160 180;
    car `CarBigPassenger 240 80;
    car `CarBigLivestock 240 100;
    car `CarBigGrain 240 120;
    car `CarBigSteel 240 140;
    car `CarBigWood 240 160;

    hash
end

module Misc = struct

  let load win res =
    let ndarray = Hashtbl.find res.Resources.res_pics "TRACKS" in
    let hash = Hashtbl.create 10 in

    let tex key x y =
      let tex = Ndarray.get_slice [[y; y + 20 - 1]; [x; x + 20 - 1]] ndarray |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in

    tex `Cursor 100 300;
    tex (`SmokeTop 1) 220 100;
    tex (`SmokeTop 2) 240 100;
    tex (`SmokeTop 3) 260 100;
    tex (`SmokeTop 4) 280 100;

    let ndarray = Hashtbl.find res.Resources.res_pics "SPRITES" in
    let tex key x y x2 y2 =
      Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] ndarray |> R.Texture.make win
      |> Hashtbl.replace hash key
    in

    tex `Newspaper 240 120 280 160;

    tex (`SmokeSide 1) 240 162 286 169;
    tex (`SmokeSide 2) 240 170 286 177;
    tex (`SmokeSide 3) 240 178 286 185;
    tex (`SmokeSide 4) 240 186 286 193;

    tex `FrameTL 281 121 289 129;
    tex `FrameTR 311 121 319 129;
    tex `FrameBL 281 151 289 159;
    tex `FrameBR 311 151 319 159;

    tex `Escape1 287 162 309 177;
    tex `Escape2 287 178 309 193;

    let ndarray = Hashtbl.find res.Resources.res_pics "LOCOS" in
    let tex key x y =
      let tex = Ndarray.get_slice [[y; y + 15 - 1]; [x; x + 80 - 1]] ndarray |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    tex (`SmokeSideBig 1) 160 49;
    tex (`SmokeSideBig 2) 160 65;
    tex (`SmokeSideBig 3) 241 49;
    tex (`SmokeSideBig 4) 241 65;
    hash

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

