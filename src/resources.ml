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
let load_sprites res =
  let open Gmap in
  let tiles = Hashtbl.create 40 in
  let small_tiles = Hashtbl.create 40 in

  let pair_fn tiles ndarray y mult key i =
    let x1, x2 = i * mult, (i+1) * mult in
    let p1 = Ndarray.get_slice [[x1; x2]; [y; y + mult]] ndarray in
    let p2 = Ndarray.get_slice [[x1; x2]; [y+mult; y + 2 * mult]] ndarray in
    Hashtbl.replace tiles key @@ Pair(p1, p2)
  in
  let single_fn tiles ndarray y mult key i =
    let x1, x2 = i * mult, (i+1) * mult in
    let p1 = Ndarray.get_slice [[x1; x2]; [y; y + mult]] ndarray in
    Hashtbl.replace tiles key @@ Single p1
  in

  let us_ndarray = Hashtbl.find res.res_pics "SPRITES" in
  let en_ndarray = Hashtbl.find res.res_pics "ESPRITES" in
  let eu_ndarray = Hashtbl.find res.res_pics "CSPRITES" in

  let load_us_tiles tiles ndarray (y:int) (mult:int) =
    let pair = pair_fn tiles ndarray y mult in
    let top = single_fn tiles ndarray y mult in
    let bottom = single_fn tiles ndarray (y+mult) mult in
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
  load_us_tiles tiles us_ndarray 0 16;
  load_us_tiles small_tiles us_ndarray 32 8;

  let load_en_tiles tiles ndarray y mult =
    let top = single_fn tiles ndarray y mult in
    let bottom = single_fn tiles ndarray (y+mult) mult in
    top Brewery 10;
    top SheepFarm 11;
    top GlassWorks 12;
    bottom SaltMine 11;
  in
  load_en_tiles tiles en_ndarray 0 16;
  load_en_tiles small_tiles en_ndarray 32 8;

  let load_eu_tiles tiles ndarray y mult =
    let top = single_fn tiles ndarray y mult in
    top Winery 10;
    top Fort 11;
    top Vinyard 13;
  in
  load_eu_tiles tiles eu_ndarray 0 16;
  load_eu_tiles small_tiles eu_ndarray 32 8;







  




