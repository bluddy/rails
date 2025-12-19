open Containers
module Ndarray = Owl_base_dense_ndarray.Generic

module R = Renderer
module C = Constants

module TileTex = struct
  module HT = Tile.DirHashtbl

  type t =
    | Single of R.Texture.t
    | Pair of R.Texture.t * R.Texture.t
    | Localized of {us: t; england: t; europe: t}

  (* Load and separate sprites *)
  let slice_tiles win res =
    let open Tilemap in
    let tiles = HT.create 40 in
    let small_tiles = HT.create 40 in

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
      HT.replace tiles key p
    in
    let single_fn ~tiles ~ndarray ~y ~mult key i =
      let x1, x2 = i * mult, (i+1) * mult - 1 in
      let y1, y2 = y, y + mult - 1 in
      (* Printf.printf "x[%d-%d] y[%d-%d]\n" x1 x2 y (y + mult); *)
      let p1 =
        Ndarray.get_slice [[y1; y2]; [x1; x2]] ndarray |> R.Texture.make win
      in
      HT.replace tiles key @@ Single p1
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
      bottom ChemicalPlant 12;
      bottom Tile.default_enemy_station 13;
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
      HT.replace tiles key @@ Localized{us;england;europe}
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
        HT.replace tiles (key @@ Dir.Set.of_list dirs) @@ Single slice
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
          HT.find hash tile
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

    let get_tex extra i j =
      (* Slice an image and turn it into a texture *)
      let x, y = j * tile_w, i * tile_h in
      let ndarr = if extra then extra_ndarray else ndarray in
      let slice = Ndarray.get_slice [[y; y + tile_h - 1]; [x; x + tile_w - 1]] ndarr in
      let tex = R.Texture.make win slice in
      tex
    in
    let load_textures ?(extra=false) start_i start_j kind dirslist =
      List.fold_left (fun (i,j) li ->
        let tex = get_tex extra i j in
        let dirs = Dir.Set.of_list li in
        let track = Track.make dirs kind C.player in
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
    load_textures 0 0 (Track `Single) all_tracks;
    load_textures 0 0 (Track `Double) all_tracks;
    load_textures ~extra:true 0 0 (Ferry `Single) all_tracks;
    load_textures ~extra:true 0 0 (Ferry `Double) all_tracks;

    load_textures 3 0 Tunnel Resources.straight_track;
    load_textures 3 4 (Bridge(Bridge.Iron)) Resources.straight_track;
    load_textures 3 8 (Bridge(Bridge.Wood)) Resources.straight_track;
    load_textures ~extra:true 3 8 (Bridge(Bridge.Stone)) Resources.straight_track;

    load_textures 4 0 (Station(`SignalTower)) Resources.straight_track;
    load_textures 4 0 (Station(`Depot)) Resources.straight_track;
    load_textures 4 4 (Station(`Station)) Resources.straight_track;
    load_textures 4 8 (Station(`Terminal)) Resources.straight_track;
    track_dict

  let find track_h track =
    Track.Htbl.find track_h track

end

module StationLights = struct
  let tile_w, tile_h = 20, 20

  let load win res =
    let ndarray = Hashtbl.find res.Resources.res_pics "TRACKS_extra" in
    let light_dict = Hashtbl.create 20 in

    let get_tex i j =
      (* Slice an image and turn it into a texture *)
      let x, y = j * tile_w, i * tile_h in
      let slice = Ndarray.get_slice [[y; y + tile_h - 1]; [x; x + tile_w - 1]] ndarray in
      let tex = R.Texture.make win slice in
      tex
    in
    let row_end = 3 in
    let load_textures start_i start_j kind =
      ignore @@ List.fold_left (fun (i,j) dir ->
        let tex = get_tex (start_i + i) (start_j + j) in
        Hashtbl.replace light_dict (dir, kind) tex;
        if j >= row_end then
          (i + 1, 0)
        else
          (i, j + 1)
      )
      (0, 0)
      Dir.dirlist
    in
    load_textures 4 0 `SignalTower;
    load_textures 4 0 `Depot;
    load_textures 4 4 `Station;
    load_textures 4 8 `Terminal;
    light_dict

  let find track_h track =
    Hashtbl.find track_h track

end

module StationTex = struct
  type hash =
    [ `Background
    | `Barn
    | `Cold
    | `Depot
    | `EngineShop
    | `Fence
    | `Goods
    | `Goods_bottom
    | `Hotel
    | `PostOffice
    | `Post_top
    | `Rest_bottom
    | `Restaurant
    | `Smokestacks
    | `Station
    | `SwitchingYard
    | `Terminal ]

  let load win res =
    let hash_us = Hashtbl.create 20 in
    let hash_en = Hashtbl.create 20 in

    let station hash prefix =
      let ndarray = Hashtbl.find res.Resources.res_pics @@ prefix^"STATION" in
      let tex key x y x2 y2 =
        let tex = 
          Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] ndarray |> R.Texture.make win
        in
        Hashtbl.replace hash key tex
      in
      tex `Background 0 141 320 200;
      tex `Depot  6 10 102 42;
      tex `Station 112 27 209 83;
      tex `Terminal 216 13 320 123;
      tex `SwitchingYard 105 2 215 18;
      tex `EngineShop 1 114 50 137;
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
    in
    station hash_us "";
    station hash_en "E";
    hash_us, hash_en
end

module CarsTop = struct
  type hash =
  | Engine of Engine._type
  | Car of Freight.t

  let tile_w, tile_h = 20, 20

  let load win res =
    let ndarray = Hashtbl.find res.Resources.res_pics "TRACKS" in
    let width = (Ndarray.shape ndarray).(1) in
    let hash = Hashtbl.create 100 in

    let get_tex x y =
      Ndarray.get_slice [[y; y + tile_h - 1]; [x; x + tile_w - 1]] ndarray
      |> R.Texture.make win
    in
    let tex x y kind =
      List.fold_left (fun x dir ->
        let tex = get_tex x y in
        Hashtbl.replace hash (kind,dir) tex;
        (x + tile_w) mod width (* handle edge of image *)
      )
      x
      Dir.dirlist
      |> ignore
    in

    (* Note: some of the textures are offset by 1 in the y direction *)
    tex 200 119 @@ Engine(Engine.SteamBig);
    tex 200 139 @@ Engine(Engine.SteamSmall);
    tex 40 120 @@ Engine(Engine.Diesel);
    tex 40 140 @@ Car(`Mail);
    tex 200 159 @@ Car(`Passenger);
    tex 40 160 @@ Car(`Fast);
    tex 200 179 @@ Car(`Slow);
    tex 40 180 @@ Car(`Bulk);
    hash
end

module RouteScreen = struct
  let load win res =
    let small_engine_hash = Hashtbl.create 10 in
    let engine_hash = Hashtbl.create 10 in
    let car_hash = Hashtbl.create 10 in

    let tex_full ~arr ~hash ~dx ~dy key x y =
      let tex = Ndarray.get_slice [[y; y + dy - 1]; [x; x + dx - 1]] arr |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    let ndarray = Hashtbl.find res.Resources.res_pics "SPRITES" in
    let tex white =
      let tt = tex_full ~arr:ndarray ~dx:63 ~dy:9 in
      if white then tt ~hash:engine_hash
      else tt ~hash:small_engine_hash
    in
    let open Engine in
    (* US *)
    let engines x white =
      tex white Grasshopper x 96;  (*  background *)
      tex white Norris x 106;
      tex white American x 116;
      tex white Mogul x 126;
      tex white TenWheeler x 136;
      tex white Consolidation x 146;
      tex white Pacific x 156;
      tex white Mikado x 166;
      tex white Mallet x 176;
      tex white FSeriesDiesel x 186;
      tex white GPSeriesDiesel x 186;
    in
    engines 0 true;
    engines 63 false;

    (* Britain *)
    let ndarray = Hashtbl.find res.Resources.res_pics "ESPRITES" in
    let tex white =
      let tt = tex_full ~arr:ndarray ~dx:63 ~dy:9 in
      if white then tt ~hash:engine_hash
      else tt ~hash:small_engine_hash
    in
    let engines x white =
      tex white Planet x 96;  (*  background *)
      tex white Patentee x 106;
      tex white IronDuke x 116;
      tex white DxGoods x 126;
      tex white Stirling x 136;
      tex white MidlandSpinner x 146;
      tex white WebbCompound x 156;
      tex white ClaudHamilton x 166;
      tex white A1Class x 176;
      tex white A4Class x 186;
    in
    engines 0 true;
    engines 63 false;

    (* Europe *)
    let ndarray = Hashtbl.find res.Resources.res_pics "CSPRITES" in
    let tex white =
      let tt = tex_full ~arr:ndarray ~dx:63 ~dy:9 in
      if white then tt ~hash:engine_hash
      else tt ~hash:small_engine_hash
    in
    let engines x white =
      tex white ClassCrocodile x 136;
      tex white ClassE18 x 146;
      tex white R242A1 x 156;
      tex white V200BB x 166;
      tex white BoBoBo x 176;
      tex white TGV x 186;
    in
    engines 0 true;
    engines 63 false;

    (* Cars *)
    let open Goods in
    let ndarray = Hashtbl.find res.Resources.res_pics "SPRITES" in
    let tex_part = tex_full ~arr:ndarray ~hash:car_hash ~dy:8 in
    let tex = tex_part ~dx:20 in

    tex (`Freight `Mail) 139 97;
    tex (`Freight `Passenger) 139 107;
    tex (`Freight `Fast) 139 117;
    tex (`Freight `Slow) 139 127;
    tex (`Freight `Bulk) 139 137;
    tex `Caboose 200 97;

    let cars ~y ~dx car_age_fn =
      let x = 160 in
      let tex car =
        tex_full ~arr:ndarray ~hash:car_hash ~dx ~dy:8 (car_age_fn car)
      in
      tex Mail x y;
      tex Passengers x (y+10);
      tex Food x (y+20);
      tex Livestock (x+dx) (y+20);
      tex MfgGoods (x+2*dx) (y+20);
      tex Grain x (y+30);
      tex Paper (x+dx) (y+30);
      tex Steel (x+2*dx) (y+30);
      tex Petroleum x (y+40);
      tex Wood (x+dx) (y+40);
      tex Coal (x+2*dx) (y+40);
    in
    cars ~y:97 ~dx:20 (fun x -> `CarOld x);
    cars ~y:147 ~dx:24 (fun x -> `CarNew x);

    (* Britain *)
    let ndarray = Hashtbl.find res.Resources.res_pics "ESPRITES" in

    let cars ~y ~dx car_age_fn =
      let x = 160 in
      let tex car =
        tex_full ~arr:ndarray ~hash:car_hash ~dx ~dy:8 (car_age_fn car)
      in
      tex Beer x (y+20);
      tex Hops (x+dx) (y+20);
      tex Textiles (x+dx) (y+30);
      tex Chemicals x (y+40);
      tex Cotton (x+dx) (y+40);
    in
    cars ~y:97 ~dx:20 (fun x -> `CarOld x);
    cars ~y:147 ~dx:24 (fun x -> `CarNew x);

    (* Europe *)
    let ndarray = Hashtbl.find res.Resources.res_pics "CSPRITES" in

    let cars ~y ~dx car_age_fn =
      let x = 160 in
      let tex car =
        tex_full ~arr:ndarray ~hash:car_hash ~dx ~dy:8 (car_age_fn car)
      in
      tex Wine x (y+20);
      tex Grapes (x+dx) (y+20);
      tex Armaments (x+2*dx) (y+20);
      tex Fertilizer x (y+30);
      tex Nitrates x (y+40);
      tex Wool (x+dx) (y+40);
    in
    cars ~y:97 ~dx:20 (fun x -> `CarOld x);
    cars ~y:147 ~dx:24 (fun x -> `CarNew x);

    small_engine_hash, engine_hash, car_hash
end

module EngineDetail = struct
  let load win res =
    let hash = Hashtbl.create 10 in

    let tex key y name =
      let ndarray = Hashtbl.find res.Resources.res_pics name in
      let tex = Ndarray.get_slice [[y; y + 50 - 1]; [0; 319]] ndarray
        |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    let open Engine in
    (* US *)
    tex Grasshopper 0 "LOCOS0";
    tex Norris 50 "LOCOS0";
    tex American 100 "LOCOS0";
    tex Mogul 150 "LOCOS0";
    tex TenWheeler 0 "LOCOS1";
    tex Consolidation 50 "LOCOS1";
    tex Pacific 100 "LOCOS1";
    tex Mikado 150 "LOCOS1";
    tex Mallet 0 "LOCOS2";
    tex FSeriesDiesel 50 "LOCOS2";
    tex GPSeriesDiesel 100 "LOCOS2";

    (* Europe *)
    tex Planet 0 "ELOCOS0";
    tex Patentee 50 "ELOCOS0";
    tex IronDuke 100 "ELOCOS0";
    tex DxGoods 150 "ELOCOS0";
    tex Stirling 0 "ELOCOS1";
    tex MidlandSpinner 50 "ELOCOS1";
    tex WebbCompound 100 "ELOCOS1";
    tex ClaudHamilton 150 "ELOCOS1";
    tex A1Class 0 "ELOCOS2";
    tex A4Class 50 "ELOCOS2";
    tex ClassCrocodile 100 "ELOCOS2";
    tex ClassE18 150 "ELOCOS2";
    tex R242A1 0 "ELOCOS3";
    tex V200BB 50 "ELOCOS3";
    tex BoBoBo 100 "ELOCOS3";
    tex TGV 150 "ELOCOS3";
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
    let hash_engine = Hashtbl.create 10 in

    let ndarray = Hashtbl.find res.Resources.res_pics "LOCOS" in

    let slice_full ~arr x y x2 y2 =
      Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] arr |> R.Texture.make win
    in
    let engine_full kind ?(anim_x=0) ?(anim_y=0) ?smoke_x ?(anim=[||]) ~arr x y x2 y2 =
      let tex = slice_full ~arr x y x2 y2 in
      let w, h = x2 - x, y2 - y in
      let engine = {
        tex; w; h; anim_x; anim_y; anim; smoke_x
      }
      in
      Hashtbl.add hash_engine kind engine
    in
    let slice = slice_full ~arr:ndarray in
    let engine = engine_full ~arr:ndarray in

    engine Engine.Grasshopper 0 5 32 24 ~anim_x:10 ~anim_y:0 ~smoke_x:15
      ~anim: [|
        slice 140 5 159 24;
        slice 140 29 159 48;
      |];
    engine Engine.Norris 0 29 49 48 ~anim_x:23 ~anim_y:12 ~smoke_x:41
      ~anim: [|
        slice 109 177 133 184;
        slice 109 185 133 192;
        slice 135 177 159 184;
        slice 135 185 159 192;
      |];
    engine Engine.American 0 53 74 72 ~anim_x:31 ~anim_y:10 ~smoke_x:59
      ~anim: [|
        slice 90 77 123 86;
        slice 90 87 123 96;
        slice 125 77 158 86;
        slice 125 87 158 96;
      |];
    engine Engine.Mogul 0 76 86 96 ~anim_x:38 ~anim_y:13 ~smoke_x:70
      ~anim: [|
        slice 34  9 73 16;
        slice 34 17 73 24;
        slice 75  9 114 16;
        slice 75 17 114 24;
      |];
    engine Engine.TenWheeler 0 98 85 120 ~anim_x:44 ~anim_y:15 ~smoke_x:70
      ~anim: [|
        slice 86 104 119 111;
        slice 86 113 119 120;
        slice 126 104 159 111;
        slice 126 113 159 120;
      |];
    engine Engine.Consolidation 0 123 89 144 ~anim_x:44 ~anim_y:136 ~smoke_x:73
      ~anim: [|
        slice 80 55 118 63;
        slice 80 64 118 73;
        slice 120 55 158 63;
        slice 120 64 158 73;
      |];
    engine Engine.Pacific 0 149 116 168 ~anim_x:70 ~anim_y:10 ~smoke_x:105
      ~anim: [|
        slice 53 29 95 38;
        slice 53 39 95 48;
        slice 96 29 138 38;
        slice 96 39 138 48;
      |];
    engine Engine.Mikado 0 175 106 192 ~anim_x:66 ~anim_y:10 ~smoke_x:94
      ~anim: [|
        slice 90 129 124 136;
        slice 90 137 124 144;
        slice 125 129 159 136;
        slice 125 137 159 144;
      |];
    engine Engine.Mallet 160 2 319 24;
    engine Engine.FSeriesDiesel 160 27 319 48;
    (* Seems like it's the same sprite *)
    engine Engine.GPSeriesDiesel 160 27 319 48;

    (* British/Euro engines *)

    let ndarray = Hashtbl.find res.Resources.res_pics "ELOCOS" in
    let slice = slice_full ~arr:ndarray in
    let engine = engine_full ~arr:ndarray in

    engine Engine.Planet 0 5 38 24 ~anim_x:2 ~anim_y:12 ~smoke_x:33
      ~anim:[|
        slice 90 2 121 9;
        slice 90 10 121 17;
        slice 122 2 153 9;
        slice 122 10 153 17;
      |];
    engine Engine.Patentee 0 28 43 48 ~anim_x:3 ~anim_y:17 ~smoke_x:37
      ~anim:[|
        slice 52 2 88 5;
        slice 52 6 88 9;
      |];
    engine Engine.IronDuke 0 53 62 72 ~anim_x:3 ~anim_y:10 ~smoke_x:54
      ~anim:[|
        slice 102 53 158 62;
        slice 102 63 158 72;
      |];
    engine Engine.DxGoods 0 80 63 96 ~anim_x:3 ~anim_y:13 ~smoke_x:56
      ~anim:[|
        slice 47 29 102 32;
        slice 47 33 102 36;
        slice 103 29 158 32;
        slice 103 33 158 36;
      |];
    engine Engine.Stirling 0 100 81 120 ~anim_x:36 ~anim_y:13 ~smoke_x:69
      ~anim:[|
        slice 74 74 115 81;
        slice 74 82 115 89;
        slice 117 74 158 81;
        slice 117 82 158 89;
      |];
    engine Engine.MidlandSpinner 0 125 78 144 ~anim_x:35 ~anim_y:13 ~smoke_x:66
      ~anim:[|
        slice 47 10 88 16;
        slice 47 17 88 23;
      |];
    engine Engine.WebbCompound 0 152 75 168 ~anim_x:29 ~anim_y:10 ~smoke_x:69
      ~anim:[|
        slice 78 38 118 44;
        slice 78 45 118 51;
        slice 119 38 159 44;
        slice 119 45 159 51;
      |];
    engine Engine.ClaudHamilton 0 176 69 192 ~anim_x:29 ~anim_y:11 ~smoke_x:59
      ~anim:[|
        slice 85 90 121 95;
        slice 85 96 121 101;
        slice 122 90 158 95;
        slice 122 96 158 101;
      |];
    engine Engine.A1Class 160 10 256 24 ~anim_x:52 ~anim_y:7 ~smoke_x:83
      ~anim:[|
        slice 70 177 114 184;
        slice 70 185 114 192;
        slice 115 177 159 184;
        slice 115 185 159 192;
      |];
    engine Engine.A4Class 160 33 255 48 ~anim_x:51 ~anim_y:10 ~smoke_x:82
      ~anim:[|
        slice 79 165 118 170;
        slice 79 171 118 176;
        slice 119 165 158 170;
        slice 119 171 158 176;
      |];

    (* European Engines *)

    let ndarray = Hashtbl.find res.Resources.res_pics "CLOCOS" in
    let slice = slice_full ~arr:ndarray in
    let engine = engine_full ~arr:ndarray in

    engine Engine.ClassCrocodile 0 103 55 120 ~anim_x:2 ~anim_y:12
      ~anim:[|
        slice 57 98 107 103;
        slice 57 104 107 109;
        slice 108 98 158 103;
        slice 108 104 158 109;
      |];
    engine Engine.ClassE18 0 124 74 144;
    engine Engine.R242A1 0 154 86 168 ~anim_x:44 ~anim_y:7 ~smoke_x:74
      ~anim:[|
        slice 82 129 120 136;
        slice 82 137 120 144;
        slice 121 129 159 136;
        slice 121 137 159 144;
      |];
    engine Engine.V200BB 0 172 91 192;
    engine Engine.BoBoBo 160 4 253 24 ~anim_x:8 ~anim_y:14
      ~anim:[|
        slice 82 114 159 120;
        slice 82 121 159 127;
      |];
    engine Engine.TGV 160 30 261 48;

    (* Cars *)
    let hash_car = Hashtbl.create 10 in

    let load_cars () =
      let get_ndarray prefix suffix = 
        Hashtbl.find res.Resources.res_pics @@ prefix^"LOCOS"^suffix
      in
      let get_ndarrays prefix =
        get_ndarray prefix "", get_ndarray prefix "M"
      in
      let ndarray_old, ndarray_new = get_ndarrays ""
      in
      let car_full ?(h=19) ?(w=47) ~arr x y =
        let y2 = y + h in
        let x2 = x + w in
        let tex = slice_full ~arr x y x2 y2 in
        tex
      in
      let car ~arrs kind x y=
        let ndarray_old, ndarray_new = arrs in
        let car_old = car_full ~arr:ndarray_old x y in
        let car_new = car_full ~arr:ndarray_new x y in
        Hashtbl.add hash_car kind (car_old, car_new)
      in
      let open Goods in
      let car2 = car ~arrs:(ndarray_old, ndarray_new) in
      car2 Mail 160 80;
      car2 Passengers 240 80;
      car2 Food 160 100;
      car2 Livestock 240 100;
      car2 MfgGoods 160 120;
      car2 Grain 240 120;
      car2 Paper 160 140;
      car2 Steel 240 140;
      car2 Petroleum  160 160;
      car2 Wood 240 160;
      car2 Coal  160 180;

      (* Britain *)
      let ndarray_old, ndarray_new = get_ndarrays "E" in
      let car2 = car ~arrs:(ndarray_old, ndarray_new) in

      car2 Beer 160 100;
      car2 Hops 240 120;
      car2 Textiles 160 140;
      car2 Chemicals 160 160;
      car2 Cotton 240 160;

      (* Europe *)
      let ndarray_old, ndarray_new = get_ndarrays "C" in
      let car = car ~arrs:(ndarray_old, ndarray_new) in

      car Wine 160 100;
      car Grapes 240 100;
      car Armaments 160 120;
      car Fertilizer 240 120;
      car Nitrates 160 160;
      car Wool 240 160;
    in
    load_cars ();

    hash_engine, hash_car
end

module Opponents = struct
  let load win res =
    let hash = Hashtbl.create 10 in
    let ndarray = Hashtbl.find res.Resources.res_pics "FACES" in
    let tex key x y =
      let tex = Ndarray.get_slice [[y; y + 45 - 1]; [x; x + 40 - 1]] ndarray |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    let open Opponent in
    let y = 0 in
    let x = ref 0 in
    let incr_x x = let x1 = !x in x := !x + 40; x1 in
    tex CorneliusVanderbilt (incr_x x) y;
    tex DanielDrew (incr_x x) y;
    tex JimFisk (incr_x x) y;
    tex JayGould (incr_x x) y;
    tex JayCooke (incr_x x) y;
    tex JPierpontMorgan (incr_x x) y;
    tex JohnForbes (incr_x x) y;
    tex JimHill (incr_x x) y;
    let y = 50 in
    let x = ref 0 in
    tex CzarNicholasII (incr_x x) y;
    tex VILenin (incr_x x) y;
    tex CharlesDeGaulle (incr_x x) y;
    tex NapoleonIII (incr_x x) y;
    tex OttoVonBismarck (incr_x x) y;
    tex BenitoMussolini (incr_x x) y;
    tex GeorgeStephenson (incr_x x) y;
    tex RobertStephenson (incr_x x) y;
    let y = 99 in
    let x = ref 0 in
    tex IsambardKBrunel (incr_x x) y;
    tex GeorgeSHudson (incr_x x) y;
    tex HelmuthVonMoltke (incr_x x) y;
    tex ErastusCorning (incr_x x) y;
    tex JEdgarThompson (incr_x x) y;
    tex BaronRothschild (incr_x x) y;
    hash
end

module Job = struct
  let load win res =
    let hash = Hashtbl.create 10 in
    let tex key x y x2 y2 suffix =
      let ndarray = Hashtbl.find res.Resources.res_pics @@ "PAGE"^suffix in
      let tex = Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] ndarray |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    tex `Surveyor 214 0 320 140 "0";
    tex `SafariLeader 92 0 214 123 "0";
    tex `ChiefOfSecretService 0 0 94 123 "0";
    tex `PolarExplorer 0 122 202 200 "0";
    tex `Hobo 0 0 218 61 "1";
    tex `NewspaperEditor 110 61 211 199 "1";
    tex `Mayor 210 62 320 200 "1";
    tex `Inventor 0 61 108 200 "1";
    tex `BankPresident 166 0 320 97 "2";
    tex `Congressman 166 102 320 199 "2";
    tex `SteamshipOwner 2 98 165 198 "2";
    tex `PresidentOfStockExchange 2 2 165 96 "2";
    tex `RiverboatGambler 176 115 320 200 "3";
    tex `ArmyCaptain 0 0 99 169 "3";
    tex `Conductor 102 115 177 200 "3";
    tex `IndianAgent 100 1 197 114 "3";
    tex `SnakeOilPeddler 199 2 320 115 "3";
    tex `CircusImpresario 0 100 156 200 "4";
    tex `BankPresident 157 101 320 200 "4";
    tex `PresidentOfTheRRTrust 157 3 320 100 "4";
    tex `StateGovernor 0 0 157 99 "4";
    tex `PresidentOfUnitedStates 0 0 131 200 "5";
    tex `GeneralOfArmies 158 0 320 100 "5";
    tex `SteamboatCaptain 158 99 320 200 "5";
    tex `StableMaster 152 102 320 200 "6";
    tex `Harbormaster 152 0 320 103 "6";
    tex `PrimeMinister 0 9 153 200 "6";
    tex `ConsultingDetective 0 0 156 105 "7";
    tex `DancingInstructor 155 0 220 168 "7";
    tex `Butler 219 0 320 143 "7";
    tex `ArmamentsMerchant 0 104 156 200 "7";
    tex `Magistrate 0 0 199 80 "8";
    tex `GameWarden 2 79 113 200 "8";
    tex `NavyCommodore 113 85 219 200 "8";
    tex `RoyalAdvisor 218 67 320 200 "8"; (* ? *)
    tex `ChimneySweep 0 0 94 131 "9";
    tex `MinisterOfFinance 178 0 320 99 "9";
    tex `ArmyInspectorGeneral 0 130 179 200 "9";
    tex `InvestmentBanker 178 98 320 200 "9";
    tex `GardenClubChairman 94 0 179 131 "9";
    hash
end

module Misc = struct
  type t =
  [ `Advert
  | `Council
  | `Credits
  | `Cursor
  | `PaperTear1
  | `PaperTear2
  | `WaterOnBridge
  | `Escape1
  | `Escape2
  | `FrameBL
  | `FrameBR
  | `FrameTL
  | `FrameTR
  | `Logo
  | `LogoMPS
  | `LogoMicroprose
  | `MainMenu of Region.t
  | `MainMenuBackground
  | `MainMenuMan of int
  | `Newspaper
  | `SideTrack 
  | `Title ]

  type smoke =
  [ `SmokeTop
  | `SmokeSide
  | `SmokeSideBig ]

  let load win res =
    let ndarray = Hashtbl.find res.Resources.res_pics "TRACKS" in
    let hash = Hashtbl.create 10 in
    let smoke_hash = Hashtbl.create 10 in

    let tex key x y =
      let tex = Ndarray.get_slice [[y; y + 20 - 1]; [x; x + 20 - 1]] ndarray |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    let slice l =
      List.map (fun (x, y) -> Ndarray.get_slice [[y; y + 20 - 1]; [x; x + 20 - 1]] ndarray
      |> R.Texture.make win) l |> Array.of_list
    in

    tex `Cursor 300 100;
    tex `PaperTear1 40 100;
    tex `PaperTear2 60 100;
    tex `WaterOnBridge 200 100;
    slice [220, 100; 240, 100; 260, 100; 280, 100] |> Hashtbl.replace smoke_hash `SmokeTop;

    let ndarray = Hashtbl.find res.Resources.res_pics "SPRITES" in
    let tex key x y x2 y2 =
      Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] ndarray |> R.Texture.make win
      |> Hashtbl.replace hash key
    in
    let slice l =
      List.map (fun (x, y, x2, y2) -> Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] ndarray
      |> R.Texture.make win) l |> Array.of_list
    in

    tex `Newspaper 240 120 280 160;

    slice [240, 62, 286, 169; 240, 170, 286, 177; 240, 178, 286, 185; 240, 186, 286, 193]
      |> Hashtbl.replace smoke_hash `SmokeSide;

    tex `FrameTL 281 121 289 129;
    tex `FrameTR 311 121 319 129;
    tex `FrameBL 281 151 289 159;
    tex `FrameBR 311 151 319 159;

    tex `Escape1 287 162 309 177;
    tex `Escape2 287 178 309 193;

    tex `Logo 256 63 320 119;

    let ndarray = Hashtbl.find res.Resources.res_pics "LOCOS" in
    let tex key ?(w=79) ?(h=15) x y =
      let tex = Ndarray.get_slice [[y; y + h - 1]; [x; x + w - 1]] ndarray |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    let slice l =
      List.map (fun (x, y, x2, y2) -> Ndarray.get_slice [[y; y2]; [x; x2]] ndarray
      |> R.Texture.make win) l |> Array.of_list
    in
    slice [160, 49, 239, 63;
           160, 65, 239, 78;
           241, 49, 319, 63;
           241, 65, 319, 78] |> Hashtbl.replace smoke_hash `SmokeSideBig;
    tex `SideTrack 0 24 ~w:159 ~h:3;

    let ndarray = Hashtbl.find res.Resources.res_pics "DIFFSP" in
    let tex key x y x2 y2 =
      let tex = Ndarray.get_slice [[y; y2 - 1]; [x; x2 - 1]] ndarray |> R.Texture.make win in
      Hashtbl.replace hash key tex
    in
    let open Region in
    tex (`MainMenu WestUS) 1 1 92 62;
    tex (`MainMenu EastUS) 94 1 185 62;
    tex (`MainMenu Britain) 187 1 278 62;
    tex (`MainMenu Europe) 1 63 92 124;
    tex (`MainMenuMan 1) 3 129 55 199;
    tex (`MainMenuMan 2) 94 63 146 120;
    tex (`MainMenuMan 3) 57 129 114 189;
    tex (`MainMenuMan 4) 115 121 176 200;

    let tex key filename =
      let ndarray = Hashtbl.find res.Resources.res_pics filename in
      let tex = R.Texture.make win ndarray in
      Hashtbl.replace hash key tex
    in

    tex `MainMenuBackground "DIFFS";
    tex `LogoMPS "LABS";
    tex `LogoMicroprose "LOGO";
    tex `Council "COUNCIL";
    tex `Credits "CREDITS2";
    tex `Title "TITLE";
    tex `Advert "ADVERT";

    hash, smoke_hash

end

type t = {
  pics: (string, R.Texture.t) Hashtbl.t;
  pixel: R.Texture.t; (* white pixel *)
  tiles: TileTex.t Tile.DirHashtbl.t;
  small_tiles: TileTex.t Tile.DirHashtbl.t;
  tracks: R.Texture.t Track.Htbl.t;
  station_us: (StationTex.hash, R.Texture.t) Hashtbl.t;
  station_en: (StationTex.hash, R.Texture.t) Hashtbl.t;
  cars_top: (CarsTop.hash * Dir.t, R.Texture.t) Hashtbl.t;
  small_engine: (Engine.make, R.Texture.t) Hashtbl.t;
  route_engine: (Engine.make, R.Texture.t) Hashtbl.t;
  route_cars: ([ `Caboose | `CarNew of Goods.t | `CarOld of Goods.t
               | `Freight of Freight.t ], R.Texture.t) Hashtbl.t;
  engine_detail: (Engine.make, R.Texture.t) Hashtbl.t;
  engine_anim: (Engine.make, TrainAnim.t) Hashtbl.t;
  car_anim: (Goods.t, (R.Texture.t * R.Texture.t)) Hashtbl.t;
  opponents: (Opponent.name, R.Texture.t) Hashtbl.t;
  jobs: (Jobs.t, R.Texture.t) Hashtbl.t;
  misc: (Misc.t, R.Texture.t) Hashtbl.t;
  smoke: (Misc.smoke, R.Texture.t array) Hashtbl.t;
  station_lights: (Dir.t * Station.kind, R.Texture.t) Hashtbl.t;
}

let of_resources win res =
  let pics = Hashtbl.to_iter res.Resources.res_pics
    |> Iter.map (fun (s, arr) -> s, R.Texture.make win arr)
    |> Hashtbl.of_iter
  in
  let pixel = R.Texture.make win Pic.white_pixel in
  let tiles, small_tiles = TileTex.slice_tiles win res in
  let small_engine, route_engine, route_cars = RouteScreen.load win res in
  let engine_anim, car_anim = TrainAnim.load win res in
  let station_us, station_en = StationTex.load win res in
  let misc, smoke = Misc.load win res in
  let station_lights = StationLights.load win res in
  {
    pics;
    pixel;
    tiles;
    small_tiles;
    tracks = Tracks.load win res;
    cars_top = CarsTop.load win res;
    engine_detail = EngineDetail.load win res;
    opponents = Opponents.load win res;
    station_us;
    station_en;
    jobs = Job.load win res;
    misc;
    small_engine;
    route_engine;
    route_cars;
    engine_anim;
    car_anim;
    smoke;
    station_lights;
  }


