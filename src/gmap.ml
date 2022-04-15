module Ndarray = Owl_base_dense_ndarray.Generic
open Containers

type area =
  | EastUS
  | WestUS
  | Britain
  | Europe
  [@@deriving enum, eq]

let areas = [EastUS; WestUS; Britain; Europe]

type tile =
  | Clear (* 0 *)
  | Woods
  | Swamp
  | Foothills
  | Hills
  | Mountains (* 5 *)
  | City
  | Village
  | Farm
  | Slums
  | FoodProc (* 10 *)
  | Ranch (* US *)
  | Stockyard (* US *)
  | Factory
  | GrainElev (* US, Eng *)
  | PaperMill (* 15, US *)
  | Landing of Dir.Set.t (* Light blue river *)
  | LumberMill (* US *)
  | CoalMine
  | SteelMill
  | PowerPlant (* 20 US, Europe *)
  | OilWell (* US *)
  | Refinery (* US *)
  | EnemyRR
  | River of Dir.Set.t (* directions of river *)
  | Ocean of Dir.Set.t (* 25 *) (* dirs are directions of land *)
  | Harbor

  (* Alternative *)
  | Desert
  | SaltMine (* Eng *)
  | TextileMill (* Eng, Eur *)
  | ChemicalPlant (* Eng, Eur *)
  | Brewery (* Eng *)
  | Vinyard (* Eur *)
  | Winery (* Eur *)
  | Fort (* Eur *)
  | GlassWorks (* Eng *)
  | SheepFarm (* Eng, Eur *)
  [@@deriving eq]

(* Map data type. Starts at top left *)
type t = {
  seed: int; (* 15 bit value *)
  map: tile array
}

type city = {
    name: string;
    x: int;
    y: int;
  }
  [@@deriving show]

type pixel =
  | Slum_pixel     (* 0 *)
  | Ocean_pixel
  | Clear_pixel
  | Woods_pixel
  | Harbor_pixel
  | CoalMine_pixel (* 5 *)
  | Desert_pixel
  | Foothills_pixel
  | OilWell_pixel
  | River_pixel
  | Farm_pixel     (* 10 *)
  | Hills_pixel
  | Village_pixel
  | EnemyRR_pixel
  | City_pixel
  | Mountain_pixel (* 15 *)
  [@@deriving enum, eq]

let pixel_of_tile = function
  | Slums -> Slum_pixel
  | Ocean _ -> Ocean_pixel
  | Clear -> Clear_pixel
  | Woods -> Woods_pixel
  | Harbor -> Harbor_pixel
  | CoalMine
  | LumberMill -> CoalMine_pixel
  | Desert
  | Swamp -> Desert_pixel
  | Foothills -> Foothills_pixel
  | OilWell
  | SaltMine -> OilWell_pixel
  | Landing _
  | River _ -> River_pixel
  | Ranch | Fort
  | SheepFarm (* for EU, village and oilwell pixel *)
  | GrainElev | Vinyard
  | Farm -> Farm_pixel
  | Hills -> Hills_pixel
  | Village
  | FoodProc | Brewery | Winery
  | PaperMill
  | TextileMill
  (* | Factory (duplicate) *)
  | GlassWorks
  | Stockyard -> Village_pixel
  | SteelMill
  | Refinery
  | ChemicalPlant
  | PowerPlant
  | Factory
  | City -> City_pixel
  | Mountains -> Mountain_pixel
  | EnemyRR -> EnemyRR_pixel

let map_height = 192
let map_width = 256

let calc_offset x y = y * map_width + x

let get_tile map x y = map.map.(calc_offset x y)

let set_tile map x y tile = map.map.(calc_offset x y) <- tile

  (* Get mask around a certain point based on a function
     edge: behavior along edges of map
     diag: use diagonals
    *)
let get_mask ?(n=8) ?(diag=false) ~edge ~map ~f ~x ~y =
  Iter.map (fun i ->
    (* check for diagonal *)
    if not diag && i land 1 = 1 then false else
    let x_off = x + Dir.x_offset.(i) in
    let y_off = y + Dir.y_offset.(i) in
    if x_off < 0 || x_off >= map_width || y_off < 0 || y_off >= map_height then
      edge
    else
      f (get_tile map x y)
  )
  Iter.(0--(n-1))


(* seed: 15 bits from time *)
let tile_of_pixel ~area ~x ~y ~pixel ~seed =
  let xy_random = x * 9 + y * 13 + seed in
  (* let pixel = Option.get @@ pixel_of_enum pixel in *)
  let simple_mapping = function
    | Slum_pixel -> Slums
    | Clear_pixel -> Clear
    | Woods_pixel -> Woods
    | Harbor_pixel -> Harbor
    | CoalMine_pixel -> CoalMine
    | Desert_pixel -> Swamp
    | Foothills_pixel -> Foothills
    | OilWell_pixel -> Factory (* TODO check *)
    | River_pixel -> River(Dir.Set.empty)
    | Farm_pixel -> Farm
    | Hills_pixel -> Hills
    | Village_pixel -> Village
    | EnemyRR_pixel -> EnemyRR
    | City_pixel -> City
    | Mountain_pixel -> Mountains
    | Ocean_pixel -> Ocean(Dir.Set.empty)
  in
  let complex_mapping pixel xy_random = match (pixel, xy_random) with
    | River_pixel, (0 | 2) -> Landing(Dir.Set.empty)
    | River_pixel, _ -> River(Dir.Set.empty)
    | Farm_pixel, 0 -> GrainElev
    | Farm_pixel, 3 -> Ranch
    | Farm_pixel, _ -> Farm
    | Village_pixel, 0 -> Stockyard
    | Village_pixel, 1 -> Factory
    | Village_pixel, 2 -> FoodProc
    | Village_pixel, 3 -> PaperMill
    | City_pixel, 0 -> SteelMill
    | City_pixel, 1 -> Factory
    | City_pixel, 2 -> Refinery
    | City_pixel, 3 -> PowerPlant
    | _ -> simple_mapping pixel
  in
  let upper_3bits = (xy_random lsr 5) land 7 in
  let mid_2bits = (xy_random lsr 3) land 3 in
  let low_3bits = xy_random land 7 in
  let us_tile = 
    match pixel with
    | CoalMine_pixel | OilWell_pixel ->
        let low_2bits = seed land 3 in
        let x = x + low_2bits in
        let next_2bits = (seed lsr 4) land 3 in
        let y = y / 2 + next_2bits in
        if x land 2 = y land 3 then OilWell
        else if x land 3 = y land 3 then LumberMill
        else CoalMine
    | Farm_pixel | Clear_pixel when upper_3bits = mid_2bits && low_3bits = 0 ->
          complex_mapping pixel mid_2bits
    | Farm_pixel | Clear_pixel ->
          simple_mapping pixel
    | _ when low_3bits = 0 ->
          complex_mapping pixel mid_2bits
    | _ ->
          simple_mapping pixel
  in
  let alternative_tile area tile = match area with
    | Britain ->
        begin match tile with
        | FoodProc -> Brewery
        | Ranch -> SheepFarm
        | Stockyard -> GlassWorks
        | PaperMill -> TextileMill
        | OilWell -> SaltMine
        | Refinery -> ChemicalPlant (* same image *)
        | x -> x
        end
    | Europe ->
        begin match tile with
        | FoodProc -> Winery
        | Ranch -> Fort
        | Stockyard -> SheepFarm
        | GrainElev -> Vinyard
        | PaperMill -> TextileMill
        | OilWell -> SheepFarm
        | Refinery -> ChemicalPlant (* same image *)
        | x -> x
        end
    | WestUS ->
        begin match tile with
        | Swamp -> Desert
        | x -> x
        end
    | _ -> tile
  in
  alternative_tile area us_tile

let ndarray_of_file filename =
  let arr = Pic.ndarray_of_file filename in
  (* All maps are 256*192 *)
  let ndarray = Ndarray.get_slice [[0;map_height-1]; [0;map_width-1]] arr in
  ndarray

let of_ndarray ~area ~seed ndarray =
  (* First pass: don't set directions for ocean and river *)
  let map = Array.make (map_height * map_width) @@ Ocean(Dir.Set.empty) in
  let map = {map; seed} in
  for y=0 to map_height-1 do
    for x=0 to map_width-1 do
      let v = Ndarray.get ndarray [|y; x|] in
      let pixel = Option.get_exn_or "Bad pixel" @@ pixel_of_enum v in
      let tile = tile_of_pixel ~area ~x ~y ~pixel ~seed in
      set_tile map x y tile
    done
  done;

  (* Second pass: resolve ocean and river directions *)
  let resolve_dirs ~edge ~f ~x ~y =
    let mask = get_mask ~map ~edge ~f ~x ~y in
    Dir.Set.of_mask mask
  in
  let river_func = function
    | River _
    | Landing _
    | Harbor
    | Ocean _ -> true
    | _ -> false
  in
  for y=0 to map_height-1 do
    for x=0 to map_width-1 do
      let tile =
        match get_tile map x y with
        | Ocean _ ->
            let dirs =
              resolve_dirs ~f:(function Ocean _ -> false | _ -> true)
                ~x ~y ~edge:false
            in
            Ocean dirs
        | River _ ->
            let dirs = resolve_dirs ~f:river_func ~x ~y ~edge:true in
            River dirs
        | Landing _ ->
            let dirs = resolve_dirs ~f:river_func ~x ~y ~edge:true in
            River dirs
        | x -> x
      in
      set_tile map x y tile
    done
  done;
  map

let of_file ~area ~seed filename =
  ndarray_of_file filename
  |> of_ndarray ~area ~seed

  (* Make an ndarray of pixel indices. Not RGBA! *)
let to_ndarray mapdata =
  let ndarray = Ndarray.empty Int8_unsigned [|map_height; map_width|] in
  let idx = ref 0 in
  for i=0 to map_height-1 do
    for j=0 to map_width-1 do
      let v = mapdata.(!idx) in
      let v = pixel_to_enum @@ pixel_of_tile v in
      Ndarray.set ndarray [|i; j|] v;
      incr idx;
    done
  done;
  ndarray

  (* Make an image RGBA ndarray *)
let to_img (map:t) =
  let ndarray = to_ndarray map.map in
  Pic.img_of_ndarray ndarray

let get_pixel ~map ~x ~y =
  get_tile map x y |> pixel_of_tile

let set_pixel ~area ~map ~x ~y ~pixel =
  let tile = tile_of_pixel ~area ~x ~y ~pixel ~seed:map.seed in
  map.map.(calc_offset x y) <- tile

