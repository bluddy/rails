open Containers

let dir = "./data/"

let map_names =
  let open Gmap in
  [
    EastUS, "EASTUS.PIC";
    WestUS, "WESTUS.PIC";
    Britain, "BRITAIN.PIC";
    Europe, "EUROPE.PIC"
  ]

let load_pics () =
  let load_ndarray s = Pic.img_of_file @@ dir ^ s ^ ".PIC" in
  let images = Hashtbl.create 20 in
  let filenames = [
    (* E and C versions are for England and Europe *)
    "SPRITES"; "CSPRITES"; "ESPRITES";
    "TRACKS"; "STATION"; "FACES"; "BRITAIN";
    "TITLE";
    "LOGO"; "LABS"; "CREDITS2"; "ADVERT";
    "DIFFS"; "DIFFSP"; "COUNCIL";
    "PAGE0"; "PAGE1"; "PAGE2"; "PAGE3"; "PAGE4"; "PAGE5"; "PAGE6"; "PAGE7"; "PAGE8"; "PAGE9";
    "LOCOS"; "CLOCOS"; "ELOCOS";
    "ELOCOS0"; "ELOCOS1"; "ELOCOS2"; "ELOCOS3"; "ELOCOSM";
    "LOCOS0"; "LOCOS1"; "LOCOS2"; "LOCOSM";
  ]
  in
  List.iter (fun s ->
    let ndarray = load_ndarray s in
    Hashtbl.replace images s ndarray)
  filenames;
  images

(* All game resources *)
type t = {
  res_maps: (Gmap.area * Gmap.t) list;
  res_pics: (string, Pic.ndarray) Hashtbl.t;
  res_cities: (Gmap.area * Gmap.city list) list;
}

let load_all ~seed =
  let res_maps =
    List.map (fun (area,s) -> area, dir ^ s
      |> Gmap.of_file ~area ~seed) map_names
  in
  let res_cities = List.map Mapgen.load_city_list Gmap.areas |> 
    List.combine Gmap.areas
  in
  let res_pics = load_pics () in
  {res_maps; res_pics; res_cities}

module Ndarray = Owl_base_dense_ndarray.Generic

type 'a tile_pics =
  | Single of 'a
  | Pair of 'a * 'a 
  | Localized of {us: 'a tile_pics; england: 'a tile_pics; europe: 'a tile_pics}

(* Load and separate sprites *)
let slice_tiles res =
  let open Gmap in
  let tiles = Hashtbl.create 40 in
  let small_tiles = Hashtbl.create 40 in

  let pair_part ndarray y mult i =
  let pair_part ~ndarray ~y ~mult ~i =
    let x1, x2 = i * mult, (i+1) * mult in
    let p1 = Ndarray.get_slice [[x1; x2]; [y; y + mult]] ndarray in
    Pair(p1, p2)
  in
  let pair_fn tiles ndarray y mult key i =
    let p = pair_part ndarray y mult i in
    Hashtbl.replace tiles key p
  in
  let single_fn tiles ndarray y mult key i =
    let x1, x2 = i * mult, (i+1) * mult in
    let p1 = Ndarray.get_slice [[x1; x2]; [y; y + mult]] ndarray in
    Hashtbl.replace tiles key @@ Single p1
  in
  in

  let us_ndarray = Hashtbl.find res.res_pics "SPRITES" in
  let en_ndarray = Hashtbl.find res.res_pics "ESPRITES" in
  let eu_ndarray = Hashtbl.find res.res_pics "CSPRITES" in

  single_fn tiles us_ndarray 40 16 Harbor 17;

  let load_us_tiles tiles (y:int) (mult:int) =
    let pair = pair_fn tiles us_ndarray y mult in
    let top = single_fn tiles us_ndarray y mult in
    let bottom = single_fn tiles us_ndarray (y+mult) mult in
    pair Clear 0;
    pair Woods 1;
    pair Swamp 2;
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
  load_us_tiles tiles 0 16;
  load_us_tiles small_tiles 32 8;

  let load_en_tiles tiles y mult =
    let top = single_fn tiles en_ndarray y mult in
    let bottom = single_fn tiles en_ndarray (y+mult) mult in
    top Brewery 10;
    top SheepFarm 11;
    top GlassWorks 12;
    bottom SaltMine 11;
  in
  load_en_tiles tiles 0 16;
  load_en_tiles small_tiles 32 8;

  let load_eu_tiles tiles y mult =
    let top = single_fn tiles eu_ndarray y mult in
    top Winery 10;
    top Fort 11;
    top Vinyard 13;
  in
  load_eu_tiles tiles 0 16;
  load_eu_tiles small_tiles 32 8;

  (* Load cities and villages, which are slightly different per area *)
  let load_localized tiles key y mult i =
    let us = pair_part us_ndarray y mult i in
    let england = pair_part en_ndarray y mult i in
    let europe = pair_part eu_ndarray y mult i in
    Hashtbl.replace tiles key @@ Localized{us;england;europe}
  in
  load_localized tiles City 0 16 6;
  load_localized tiles Village 0 16 7;
  load_localized small_tiles City 32 8 6;
  load_localized small_tiles Village 32 8 7;

  let dir_tiles = Hashtbl.create 20 in
  let small_dir_tiles = Hashtbl.create 20 in

  let load_dir_tiles ~tiles ~key ~y ~x ~mult =
    let img i l =
      let x1, x2 = x + i * mult, x + (i+1) * mult in
      let slice = Ndarray.get_slice [[x1; x2]; [y; y + mult]] us_ndarray in
      Hashtbl.replace tiles (key, Dir.Set.of_list l) slice
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
  load_dir_tiles ~tiles:dir_tiles ~key:Ocean ~y:48 ~x:0 ~mult:16;
  load_dir_tiles ~tiles:dir_tiles ~key:River ~y:64 ~x:0 ~mult:16;
  load_dir_tiles ~tiles:small_dir_tiles ~key:Ocean ~y:32 ~x:160 ~mult:8;
  load_dir_tiles ~tiles:small_dir_tiles ~key:River ~y:40 ~x:160 ~mult:8;
  
  (tiles, small_tiles, dir_tiles, small_dir_tiles)

