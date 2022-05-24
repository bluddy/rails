module Ndarray = Owl_base_dense_ndarray.Generic
open Containers

type area =
  | EastUS
  | WestUS
  | Britain
  | Europe
  [@@deriving enum, eq]

let areas = [EastUS; WestUS; Britain; Europe]

(* Map data type. Starts at top left *)
type t = {
  seed: int; (* 15 bit value *)
  map: Tile.t array;
  heightmap: int array;
  width: int;
  height: int;
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
  | EnemyRR_pixel (* dummy *)
  | City_pixel
  | Mountain_pixel (* 15 *)
  [@@deriving enum, eq]

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
  | EnemyRR_pixel -> 1
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
  | EnemyRR -> assert false

let map_height = 192
let map_width = 256

let calc_offset v x y = y * v.width + x

let x_y_of_offset ~width offset =
  let y = offset / width in
  let x = offset mod width in
  x, y

let get_tile v x y = v.map.(calc_offset v x y)

let set_tile v x y tile =
  v.map.(calc_offset v x y) <- tile

let set_height v ~x ~y height =
  v.heightmap.(calc_offset v x y) <- height

let get_tile_height v x y = v.heightmap.(calc_offset v x y)

let mapxy ~width f arr =
  Array.mapi (fun i value ->
    let x, y = x_y_of_offset ~width i in
    f x y value)
  arr

let iter f v =
  for y=0 to v.height - 1 do
    for x=0 to v.width - 1 do
      let tile = get_tile v x y in
      f x y tile
    done
  done


  (* Get mask around a certain point based on a function
     edge: behavior along edges of map
     diag: use diagonals
    *)
let get_mask ?(n=8) ?(diag=false) ~edge v ~f ~x ~y =
  Iter.map (fun i ->
    (* check for diagonal *)
    if not diag && i land 1 = 1 then false else
    let x_off = x + Dir.x_offset.(i) in
    let y_off = y + Dir.y_offset.(i) in
    if x_off < 0 || x_off >= v.width || y_off < 0 || y_off >= v.height then
      edge
    else
      f (get_tile v x_off y_off)
  )
  Iter.(0--(n-1))

(* seed: 15 bits from time *)
let tile_of_pixel ~area ~x ~y ~pixel v =
  let seed = v.seed in
  let water_dirs ~edge ~f =
    let mask = get_mask ~edge ~f ~x ~y v in
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
  (* let pixel = Option.get @@ pixel_of_enum pixel in *)
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
    | EnemyRR_pixel -> assert false
  in
  (* NOTE: Does some area change the mappings?
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
  let alternative_tile area tile = match area with
    | Britain ->
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
  alternative_tile area us_tile

let create_heightmap v =
  (* Convolution operation *)
  let convolve x y get_val =
    List.foldi (fun sum i mult ->
      let dx, dy = Dir.x_offset.(i), Dir.y_offset.(i) in
      let x2, y2 = x + dx, y + dy in
      if x2 < 0 || y2 < 0 || x2 >= v.width || y2 >= v.height then
        sum
      else
        let value = get_val x2 y2 in
        sum + mult * value)
      0
      Dir.offset_conv_map
  in
  (* Get plain height map *)
  let heightmap =
    mapxy (fun _ _ tile -> pixel_of_tile tile |> height_of_pixel)
      ~width:v.width
      v.map
  in
  (* Get convolved height map *)
  mapxy (fun x y height ->
    let other_h = convolve x y (fun x y -> heightmap.(calc_offset v x y)) in
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
  let ndarray = Ndarray.get_slice [[0;map_height-1]; [0;map_width-1]] arr in
  ndarray

let of_ndarray ~area ~seed ndarray =
  (* First pass: don't set directions for ocean and river *)
  let width = 256 in
  let height = 192 in
  let map = Array.make (width * height) @@ Tile.Ocean(Dir.Set.empty) in
  let heightmap = Array.empty in
  let v = {map; seed; width; height; heightmap} in
  for y=0 to v.height-1 do
    for x=0 to v.width-1 do
      let value = Ndarray.get ndarray [|y; x|] in
      let pixel = Option.get_exn_or "Bad pixel" @@ pixel_of_enum value in
      let tile = tile_of_pixel ~area ~x ~y ~pixel v in
      set_tile v x y tile
    done
  done;

  (* Second pass: fix up ocean and river directions *)
  for y=0 to v.height-1 do
    for x=0 to v.width-1 do
      let value = Ndarray.get ndarray [|y; x|] in
      let pixel = Option.get_exn_or "Bad pixel" @@ pixel_of_enum value in
      let tile = tile_of_pixel ~area ~x ~y ~pixel v in
      set_tile v x y tile
    done
  done;
  v


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

let set_pixel ~area v ~x ~y ~pixel =
  let tile = tile_of_pixel ~area ~x ~y ~pixel v in
  v.map.(calc_offset v x y) <- tile

let get_grade v ~dir ~x ~y =
  let dx, dy = Dir.to_offsets dir in
  let x2, y2 = x + dx, y + dy in
  let height1 = get_tile_height v x y in
  let height2 = get_tile_height v x2 y2 in
  let grade = abs(height1 - height2) in
  (* Even out grades for diagonals and non-diagonals *)
  if not (Dir.is_diagonal dir) then
    (grade * 3) / 2
  else
    grade

    (* Get the length of a tunnel needed.
       We stop when we reach the same height more or less *)
let get_tunnel_length v ~x ~y ~dir =
   let dx, dy = Dir.to_offsets dir in
   let height1div = (get_tile_height v x y) / 10 in
   let rec loop x y n =
     if x < 0 || x >= v.width || y < 0 || y >= v.height then None
     else
      match get_tile v x y with
      | Ocean _ | River _ | Landing _ | Harbor _ -> None
      | _ -> 
        let height = get_tile_height v x y in
        if height/10 <= height1div then Some n
        else
          loop (x+dx) (y+dy) (n+1)
   in
   match loop (x+dx) (y+dy) 1 with
   | Some x when x <= 1 -> None
   | x -> x

let check_build_track v ~x ~y ~dir =
   let tile1 = get_tile v x y in
   let dx, dy = Dir.to_offsets dir in
   let x2, y2 = x + dx, y + dy in
   if x2 < 0 || x2 >= v.width || y2 < 0 || y2 >= v.height then `Illegal
   else
    let tile2 = get_tile v x2 y2 in
    match tile1, tile2 with
    (* Cannot build over river, or ocean to river *)
    | Ocean _, Ocean _ -> `Ferry
    | (Ocean _, t | t, Ocean _) when Tile.is_ground t -> `Ferry
    | t1, t2 when Tile.is_ground t1 && Tile.is_ground t2 ->
        let height1 = get_tile_height v x y in
        let height2 = get_tile_height v x2 y2 in
        let grade = get_grade v ~x ~y ~dir in
        if grade > 12 then
          if height2 > height1 && height1 > 80 then
            match get_tunnel_length v ~x ~y ~dir with
            | Some length -> `Tunnel(length, grade)
            | None -> `HighGrade grade
          else
            `HighGrade grade
        else
          `Ok
    (* Bridge *)
    | t, River _ when Tile.is_ground t && Dir.is_cardinal dir ->
        let x3, y3 = x2 + dx, y2 + dy in
        if x3 < 0 || x3 >= v.width || y3 < 0 || y3 >= v.height then `Illegal
        else
          let tile3 = get_tile v x3 y3 in
          if Tile.is_ground tile3 then `Bridge
          else `Illegal
    | _, _ -> `Illegal

let check_build_station v ~x ~y =
  let tile = get_tile v x y in
  if Tile.is_ground tile then `Ok
  else `Illegal







