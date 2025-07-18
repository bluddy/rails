module Ndarray = Owl_base_dense_ndarray.Generic
open Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let src = Logs.Src.create "tilemap" ~doc:"Tilemap"
module Log = (val Logs.src_log src: Logs.LOG)

module C = Constants
module Hashtbl = Utils.Hashtbl

let map_height_default = 192
let map_width_default = 256

(* Map data type. Starts at top left *)
type t = {
  seed: int; (* 15 bit value *)
  map: Tile.t array;
  heightmap: int array;
  width: int;
  height: int;
  region: Region.t;
} [@@deriving yojson]

let default = {
  seed=0; map=[||]; heightmap=[||]; width=0; height=0; region=Region.WestUS
}

let get_height v = v.height
let get_width v = v.width

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
  | EnemyStation_pixel (* Represents enemey stations, which can only be at proper city locations *)
  | City_pixel
  | Mountain_pixel (* 15 *)
  [@@deriving enum, eq, ord, yojson]

let height_of_pixel = function
  | Slum_pixel -> 1
  | Ocean_pixel -> 0
  | Clear_pixel -> 1
  | Woods_pixel -> 1
  | Harbor_pixel -> 0
  | CoalMine_pixel -> 2
  | Desert_pixel -> 1
  | Foothills_pixel -> 2
  | OilWell_pixel -> 1
  | River_pixel -> 0
  | Farm_pixel -> 1
  | Hills_pixel -> 4
  | Village_pixel -> 1
  | EnemyStation_pixel -> 1
  | City_pixel -> 1
  | Mountain_pixel -> 8

let pixel_of_tile = function
  | Tile.Slums -> Slum_pixel
  | Ocean _ -> Ocean_pixel
  | Clear -> Clear_pixel
  | Woods -> Woods_pixel
  | Harbor _ -> Harbor_pixel
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
  | GrainElev | Vineyard
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
  | EnemyStation -> EnemyStation_pixel

let out_of_bounds_xy x y v = x < 0 || x >= v.width || y < 0 || y >= v.height
let out_of_bounds (x, y) v = out_of_bounds_xy x y v

let get_tile_xy x y v = v.map.(Utils.calc_offset v.width x y)
let get_tile (x,y) v = get_tile_xy x y v

let set_tile_xy x y tile v = v.map.(Utils.calc_offset v.width x y) <- tile
let set_tile (x,y) tile v = set_tile_xy x y tile v

let set_height (x,y) height v = v.heightmap.(Utils.calc_offset v.width x y) <- height

let get_tile_height_xy x y v = v.heightmap.(Utils.calc_offset v.width x y)
let get_tile_height (x, y) v = get_tile_height_xy x y v

  (* generic map over any array (tile or height) *)
let map_gen ~width f arr =
  Array.mapi (fun i value ->
    let x, y = Utils.x_y_of_offset width i in
    f x y value)
  arr

let map f v = map_gen ~width:v.width f v.map

  (* Fold over a range of the tilemap *)
let fold_range x y ~range ~f ~init v =
  Utils.fold_range x y ~width:v.width ~height:v.height ~read_f:(fun x y -> get_tile_xy x y v) ~range ~f ~init

  (* Get mask around a certain point based on a function
     edge: behavior along edges of map
     diag: use diagonals
    *)
let get_mask x y ?(n=8) ?(diag=false) ~edge v ~f =
  Iter.map (fun i ->
    (* check for diagonal *)
    if not diag && i land 1 = 1 then false else
    let loc2 = Dir.adjust_offset_i i ~x ~y in
    if out_of_bounds loc2 v then edge
    else
      f @@ get_tile loc2 v
  )
  Iter.(0--(n-1))

(* seed: 15 bits from time
   pixel: pixel at this position
 *)
let tile_of_pixel_xy x y ~region ~pixel v =
  let seed = v.seed in
  let water_dirs ~edge ~f =
    let mask = get_mask x y ~edge ~f v in
    Dir.Set.of_mask mask
  in
  let is_water = function
    | Tile.River _
    | Landing _
    | Harbor _
    | Ocean _ -> true
    | _ -> false
  in
  let not_water x = not (is_water x) in
  let xy_random = x * 9 + y * 13 + seed in
  let simple_mapping = function
    | Slum_pixel -> Tile.Slums
    | Clear_pixel -> Clear
    | Woods_pixel -> Woods
    | Harbor_pixel ->
        Harbor(water_dirs ~edge:false ~f:not_water)
    | CoalMine_pixel -> CoalMine
    | Desert_pixel -> Swamp
    | Foothills_pixel -> Foothills
    | OilWell_pixel -> Factory (* TODO check *)
    | River_pixel ->
        River(water_dirs ~edge:false ~f:is_water)
    | Farm_pixel -> Farm
    | Hills_pixel -> Hills
    | Village_pixel -> Village
    | City_pixel -> City
    | Mountain_pixel -> Mountains
    | Ocean_pixel ->
        Ocean(water_dirs ~edge:false ~f:not_water)
    | EnemyStation_pixel -> EnemyStation
  in
  (* NOTE: Does some region change the mappings?
      Otherwise why would clear pixels get complex mapping when they can't have anything? *)
  let complex_mapping pixel xy_random =
    (* xy_random is 0-3 *)
    match (pixel, xy_random) with
    | River_pixel, (0 | 2) ->
        Tile.Landing(water_dirs ~edge:false ~f:is_water)
    | River_pixel, _ ->
        River(water_dirs ~edge:false ~f:is_water)
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
        if x land 2 = y land 3 then Tile.OilWell
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
  let alternative_tile region tile = match region with
    | Region.Britain ->
        begin match tile with
        | Tile.FoodProc -> Tile.Brewery
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
        | GrainElev -> Vineyard
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
  alternative_tile region us_tile

let tile_of_pixel (x, y) ~region ~pixel v = tile_of_pixel_xy x y ~region ~pixel v

let create_heightmap v =
  (* Convolution operation *)
  let convolve x y get_val =
    List.foldi (fun sum i mult ->
      let (x2, y2) as loc2 = Dir.adjust_offset_i i ~x ~y in
      if out_of_bounds loc2 v then
        sum
      else
        let value = get_val x2 y2 in
        sum + mult * value)
      0
      Dir.offset_conv_map
  in
  (* Get plain height map *)
  let heightmap =
    map (fun _ _ tile -> pixel_of_tile tile |> height_of_pixel) v
  in
  (* Get convolved height map *)
  map_gen (fun x y height ->
    let other_h = convolve x y (fun x y -> heightmap.(Utils.calc_offset v.width x y)) in
    let factor = (x * y) mod 8 in
    height * 16 + other_h + factor)
    ~width:v.width
    heightmap

let update_heightmap v =
  let heightmap = create_heightmap v in
  {v with heightmap}

let ndarray_of_file filename =
  let arr = Pic.ndarray_of_file filename in
  (* All maps are 256*192 *)
  let ndarray = Ndarray.get_slice [[0;map_height_default-1]; [0;map_width_default-1]] arr in
  ndarray

let of_ndarray ?(width=256) ?(height=192) ~region ~seed ndarray =
  (* First pass: don't set directions for ocean and river *)
  let map = Array.make (width * height) @@ Tile.Ocean(Dir.Set.empty) in
  let heightmap = Array.empty in
  let v = {map; seed; width; height; heightmap; region} in
  for y=0 to v.height-1 do
    for x=0 to v.width-1 do
      let value = Ndarray.get ndarray [|y; x|] in
      let pixel = Option.get_exn_or "Bad pixel" @@ pixel_of_enum value in
      let tile = tile_of_pixel_xy x y ~region ~pixel v in
      set_tile_xy x y tile v
    done
  done;

  (* Second pass: fix up ocean and river directions *)
  for y=0 to v.height-1 do
    for x=0 to v.width-1 do
      let value = Ndarray.get ndarray [|y; x|] in
      let pixel = Option.get_exn_or "Bad pixel" @@ pixel_of_enum value in
      let tile = tile_of_pixel_xy x y ~region ~pixel v in
      set_tile_xy x y tile v
    done
  done;
  v


let of_file ~region filename =
  ndarray_of_file filename |> of_ndarray ~region 

  (* Make an ndarray of pixel indices. Not RGBA! *)
let to_ndarray mapdata =
  let ndarray = Ndarray.empty Int8_unsigned [|map_height_default; map_width_default|] in
  let idx = ref 0 in
  for i=0 to map_height_default-1 do
    for j=0 to map_width_default-1 do
      let v = mapdata.(!idx) in
      let v = pixel_to_enum @@ pixel_of_tile v in
      Ndarray.set ndarray [|i; j|] v;
      incr idx;
    done
  done;
  ndarray

  (* Map an indexed ndarray to a silhouette image *)
let to_silhouette (map:t) =
  to_ndarray map.map
  |> Ndarray.map (function 1 -> 1 | _ -> 3)
  |> Pic.img_of_ndarray ~transparent:false

  (* Make an image RGBA ndarray *)
let to_img (map:t) =
  let ndarray = to_ndarray map.map in
  Pic.img_of_ndarray ~transparent:false ndarray

let get_pixel_xy x y v = get_tile_xy x y v |> pixel_of_tile
let get_pixel (x,y) v = get_pixel_xy x y v

let set_pixel_xy x y ~region ~pixel v =
  let tile = tile_of_pixel_xy x y  ~region ~pixel v in
  v.map.(Utils.calc_offset v.width x y) <- tile
let set_pixel (x,y) ~region ~pixel v = set_pixel_xy x y ~region ~pixel v

let get_grade loc ~dir v =
  let loc2 = Dir.adjust_loc dir loc in
  let height1 = get_tile_height loc v in
  let height2 = get_tile_height loc2 v in
  let grade = abs(height1 - height2) in
  (* Even out grades for diagonals and non-diagonals *)
  (* TODO: check this *)
  if not @@ Dir.is_diagonal dir then Dir.diag_adjust_x grade else grade

    (* Get the length of a tunnel needed.
       We stop when we reach the same height more or less *)
let check_build_tunnel ((x,y) as loc) ~dir v =
   let dx, dy = Dir.to_offsets dir in
   let base_dist = Dir.diag_adjust dir C.track_length in
   let start_height = get_tile_height loc v in
   let rec loop loc n =
     if out_of_bounds loc v then `OutOfBounds
     else if n > C.tunnel_max_length then `TooLong
     else
      match get_tile loc v with
      | Ocean _ | River _ | Landing _ | Harbor _ -> `HitWater
      | _ -> 
        let height = get_tile_height loc v in
        if height <= start_height + 8 then
          (* we're done *)
          `Tunnel(n, n * base_dist, Money.(C.tunnel_cost * n * base_dist))
        else
          loop (x+dx, y+dy) (n+1)
   in
   loop (x+dx, y+dy) 1

let check_build_track loc ~dir (params:Params.t) v =
   let tile1 = get_tile loc v in
   let loc2 = Dir.adjust_loc dir loc in
   if out_of_bounds loc2 v then `Illegal
   else
    let tile2 = get_tile loc2 v in
    match tile1, tile2 with
    (* Cannot build over river, or ocean to river *)
    | Ocean _, Ocean _ -> `Ferry
    | (Ocean _, t | t, Ocean _) when Tile.is_ground t -> `Ferry
    | t1, t2 when Tile.is_ground t1 && Tile.is_ground t2 ->
        let height1 = get_tile_height loc v in
        let height2 = get_tile_height loc2 v in
        let grade = get_grade loc v ~dir in
        let grade = if B_options.easy params.options.difficulty then grade/2 else grade in
        if grade > 12 then
          if height2 > height1 && height1 > C.tunnel_min_height then
            `Tunnel grade
          else
            `HighGrade grade
        else
          `Ok
    (* Bridge *)
    | t, River _ when Tile.is_ground t && Dir.is_cardinal dir ->
        let loc3 = Dir.adjust_loc dir loc2 in
        if out_of_bounds loc3 v then `Illegal
        else
          let tile3 = get_tile loc3 v in
          if Tile.is_ground tile3 then `Bridge
          else `Illegal
    | t, EnemyStation when Tile.is_ground t -> `RateWar loc2
    | _, _ -> `Illegal

let check_build_station loc v =
  let tile = get_tile loc v in
  if Tile.is_ground tile then `Ok
  else `Illegal

  (* Collect demand and supply in the vicinity of a tile *)
let collect_demand_supply_xy x y ~range v =
  fold_range x y ~range ~init:(Hashtbl.create 10, Hashtbl.create 10) v
  ~f:(fun (demand_h, supply_h) _ _ tile ->
    let tileinfo = Tile.Info.get v.region tile in
    let collect_amount source target_h =
      List.iter (fun (goods, amount) ->
        Hashtbl.update target_h ~k:goods
          ~f:(fun _ -> function
            | None -> Some amount
            | Some a -> Some (amount + a))
      )
      source
    in
    collect_amount tileinfo.demand demand_h;
    collect_amount tileinfo.supply supply_h;
    (demand_h, supply_h)
  )
let collect_demand_supply (x, y) ~range v = collect_demand_supply_xy x y ~range v

let demand_supply_sum loc ~range v =
  let demand, supply = collect_demand_supply loc v ~range in
  (Hashtbl.sum (fun _ i -> i) demand) + (Hashtbl.sum (fun _ i -> i) supply)

 (* track_cost already includes economic climate *) 
let track_land_expense ~track_expense ~x ~y ~dir ~len v =
  let calc_cost x y =
    let tile = get_tile_xy x y v in
    let info = Tile.Info.get v.region tile in
    (* Really this is a ratio of the track expense *)
    Money.(info.cost *~ track_expense / 3)
  in
  let _, _, expense =
    Iter.fold (fun (x, y, expense) _ ->
      let cost1 = calc_cost x y in
      let x2, y2 = Dir.adjust dir x y in
      let cost2 = calc_cost x2 y2 in
      (x2, y2, Money.(expense + cost1 + cost2))
    )
    (x, y, Money.zero)
    Iter.(0 -- (len-1))
  in
  expense

  (* Check whether we can build at a tile *)
let check_build_industry_at x y wanted_tile ~region v =
  match get_pixel_xy x y v with
  | Woods_pixel | Clear_pixel | Farm_pixel | Desert_pixel ->
    let possible_tile1 = tile_of_pixel_xy ~region x y ~pixel:Village_pixel v in
    let possible_tile2 = tile_of_pixel_xy ~region x y ~pixel:City_pixel v in
    Tile.(possible_tile1 = wanted_tile || possible_tile2 = wanted_tile)
  | _ -> false

  (* Search for a site to build a specific tile (industry) *)
let search_for_industry_site x y wanted_tile ~region v =
  Utils.scan x y ~range:3 ~width:(get_width v) ~height:(get_height v)
    ~f:(fun x y -> check_build_industry_at x y wanted_tile ~region v)

