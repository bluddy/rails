module Ndarray = Owl_base_dense_ndarray.Generic

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
  | Ranch
  | Stockyard
  | Factory
  | GrainElev
  | PaperMill (* 15 *)
  | Landing
  | LumberMill
  | CoalMine
  | SteelMill
  | PowerPlant (* 20 *)
  | OilWell
  | Refinery
  | EnemyRR
  | River
  | Ocean      (* 25 *)
  | Harbor

  (* Alternative *)
  | Desert
  | SaltMine
  [@@deriving eq]

type t = tile array

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
  [@@deriving enum]

let pixel_of_tile = function
  | Slums -> Slum_pixel
  | Ocean -> Ocean_pixel
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
  | Landing
  | River -> River_pixel
  | Ranch
  | GrainElev
  | Farm -> Farm_pixel
  | Hills -> Hills_pixel
  | Village
  | FoodProc
  | PaperMill
  | Factory
  | Stockyard -> Village_pixel
  | SteelMill
  | Refinery
  | PowerPlant
  (* | Factory (duplicate) *)
  | City -> City_pixel
  | Mountains -> Mountain_pixel
  | EnemyRR -> EnemyRR_pixel

let tile_of_pixel_default = function
  | Slum_pixel -> Slums
  | Ocean_pixel -> Ocean
  | Clear_pixel-> Clear
  | Woods_pixel-> Woods
  | Harbor_pixel -> Harbor
  | Desert_pixel -> Desert
  | Foothills_pixel -> Hills
  | River_pixel -> River
  | Farm_pixel -> Farm
  | Hills_pixel -> Hills
  | Village_pixel -> Village
  | City_pixel -> City
  | Mountain_pixel -> Mountains
  | x -> failwith @@ Printf.sprintf "Illegal basic map value %d" (pixel_to_enum x)

let map_height = 192
let map_width = 256

let calc_offset x y = y * map_width + x
let read_map map x y = map.(calc_offset x y)

(* random_seed: 15 bits from time *)
let tile_of_pixel ~x ~y ~pixel ~random_seed =
  let xy_random = x * 9 + y * 13 + random_seed in
  let pixel = Option.get @@ pixel_of_enum pixel in
  let simple_mapping = function
    | Slum_pixel -> Slums
    | Ocean_pixel -> Ocean
    | Clear_pixel -> Clear
    | Woods_pixel -> Woods
    | Harbor_pixel -> Harbor
    | CoalMine_pixel -> CoalMine
    | Desert_pixel -> Swamp
    | Foothills_pixel -> Foothills
    | OilWell_pixel -> Factory
    | River_pixel -> River
    | Farm_pixel -> Farm
    | Hills_pixel -> Hills
    | Village_pixel -> Village
    | EnemyRR_pixel -> EnemyRR
    | City_pixel -> City
    | Mountain_pixel -> Mountains
  in
  let complex_mapping pixel xy_random = match (pixel, xy_random) with
  | River_pixel, (0 | 2) -> Landing
  | River_pixel, _ -> River
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
  match pixel with
  | CoalMine_pixel | OilWell_pixel ->
      let low_2bits = random_seed land 3 in
      let x = x + low_2bits in
      let next_2bits = (random_seed lsr 4) land 3 in
      let y = y / 2 + next_2bits in
      if x land 2 = y land 3 then OilWell else
      if x land 3 = y land 3 then LumberMill else
      CoalMine
  | Farm_pixel | Clear_pixel when upper_3bits = mid_2bits && low_3bits = 0 ->
        complex_mapping pixel mid_2bits
  | Farm_pixel | Clear_pixel ->
        simple_mapping pixel
  | _ when low_3bits = 0 ->
        complex_mapping pixel mid_2bits
  | _ ->
        simple_mapping pixel

let ndarray_of_file filename =
  let arr = Pic.ndarray_of_file filename in
  (* All maps are 256*192 *)
  let ndarray = Ndarray.get_slice [[0;map_height-1]; [0;map_width-1]] arr in
  ndarray

let of_ndarray ndarray =
  let map = Array.make (map_height * map_width) Ocean in
  let idx = ref 0 in
  for i=0 to map_height-1 do
    for j=0 to map_width-1 do
      let v = Ndarray.get ndarray [|i; j|] in
      let v = Option.get @@ pixel_of_enum v in
      let map_v = tile_of_pixel_default v in
      map.(!idx) <- map_v;
      incr idx;
    done
  done;
  map

let of_file filename =
  ndarray_of_file filename |> of_ndarray

let to_ndarray map =
  let ndarray = Ndarray.empty Int8_unsigned [|map_height; map_width|] in
  let idx = ref 0 in
  for i=0 to map_height-1 do
    for j=0 to map_width-1 do
      let v = map.(!idx) in
      let v = pixel_to_enum @@ pixel_of_tile v in
      Ndarray.set ndarray [|i; j|] v;
      incr idx;
    done
  done;
  ndarray

let to_img map =
  let ndarray = to_ndarray map in
  Pic.img_of_ndarray ndarray

