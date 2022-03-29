module Ndarray = Owl_base_dense_ndarray.Generic

type area =
  | EastUS
  | WestUS
  | England
  | Europe
  [@@deriving enum]

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

module MapGen = struct

let add_mountain_pixel = function
  | Foothills_pixel -> Hills_pixel
  | Hills_pixel -> Mountain_pixel
  | Ocean_pixel
  | River_pixel
  | Harbor_pixel as x -> x
  | _ -> Hills_pixel

  (* Create a list of random mountains to add to the map, based on area *)
let add_mountains_list area =
  (* Standard mountains *)
  let rec standard_range_loop j acc =
    if j >= 384 then acc
    else
      let x = Random.int 250 + 1 in (* 1 - 250 *)
      let y = Random.int 186 + 6 in (* 6 - 191 *)
      let n = Random.int 4 + 2 in (* 2 - 5 *)

      (* Normal mountain placement *)
      let rec place_mountain i x y acc =
        if i >= n then acc
        else
          (* add current and left *)
          let acc = (x, y)::(x-1,y)::acc in
          let acc = 
            (* for mid-range also add to the right *)
            if i > 1 && i <= n - 2 then (x+1, y)::acc
            else acc
          in
          (* move up for next round *)
          let y = y - 1 in
          if y < 0 then (* break if we went negative *)
            acc
          else
            let x = 
              (* randomly move right *)
              if Random.int 4 = 0 then x + 1 else x
            in
            place_mountain (i+1) x y acc
      in
      let acc = place_mountain 0 x y acc in
      standard_range_loop (j+1) acc
  in
  let mountains = standard_range_loop 0 [] in

  (* Extra ranges for US *)
  match area with
  | WestUS | EastUS ->
      (* Extra Mountains *)
      let rec extra_range_loop j acc =
        if j >= 128 then acc else

        let y = Random.int 133 + 35 in (* 35 - 167 *)
        let y =
          if y > 150 then y + Random.int 20 else y
        in
        let formula = 
          match area with
          | EastUS -> 157 - y/3
          | WestUS ->
              if y mod 4 <> 0 || y > 144 || y < 50 then
                y / 5 + 96
              else
                y / 5 + 36
          | _ -> assert false
        in
        let x = Random.int 200 - 100 in (* -100 to 99 *)
        let delta_x = Random.int 100 in
        (* Bring closer to 0 *)
        let x = if x >= 0 then x - delta_x else x + delta_x in
        let a, b = match area with
          | EastUS -> (x * 200, 400 - y)
          | WestUS -> (x * 32, 200 - formula)
          | _ -> assert false
        in
        let x = a / b + formula in
        let x, y =
          if y < 80 then
            let y = y - Random.int 35 in (* move up *)
            let x = 
              match area with
              | WestUS -> x (* check *)
              | EastUS -> x + 47 - 2 * y / 3
              | _ -> assert false
            in x, y
          else
            x, y
        in
        (* Create a large range at this location *)
        let create_range x y acc = 
          let n = Random.int 16 + 3 in
          let rec add_mountain i x y acc =
            if i >= n then acc else
            let acc = (x, y)::(x-1, y)::acc in (* current, left *)
            let acc =
              if i > 1 && i <= n-2 then
                if j mod 2 = 1 then
                  (x, y)::(x+1, y)::acc  (* current, right *)
                else
                  (x+1, y)::acc (* just right *)
              else
                acc
            in
            let y = y - 1 in (* go up *)
            if y < 0 then acc (* break if neg *)
            else
              let x = 
                (* chances of moving left and right *)
                let max = if y <= 60 then 3 else 6 in
                if Random.int max = 0 then
                  match area with
                  | EastUS -> x + 1   (* to the right *)
                  | WestUS -> x - 1   (* to the left *)
                  | _ -> assert false
                else x                (* stay where we are *)
              in
              add_mountain (i+1) x y acc
          in
          add_mountain 0 x y acc
        in
        let acc =
          match area with
          | EastUS ->
              let y = y + 38 in (* move down *)
              if y <= 191 then
                create_range x y mountains
              else
                mountains
          | WestUS ->
              create_range x y mountains
          | _ -> assert false
        in
        extra_range_loop (j+1) acc
      in
      extra_range_loop 0 mountains

  | _ -> mountains

  (* We loop until we manage to add the given resource
     This particular function needs to access the map, and since resources can only 
     be in certain areas, it may fail and need to try again and again.
   *)
let add_resource area ~map ~land_type ~resource_pixel ~wanted_tile ~random_seed =
  let rec loop () =
    let x = Random.int 256 in
    let y = Random.int 192 in
    let x = match area with
      | EastUS -> x + Random.int (319 - x)
      | WestUS when x >= 120 -> x + Random.int (255 - x)
      | WestUS -> x - Random.int x
      | England -> x + Random.int (160 - x)
      | Europe -> x
    in
    let rec attempt i x y =
      if i >= 2 then () else
      let offset = calc_offset x y in
      let tile = map.(offset) in
      let possible_tile = tile_of_pixel ~x ~y ~random_seed ~pixel:resource_pixel in
      if tile = land_type && possible_tile = wanted_tile then (
        map.(offset) <- possible_tile;
        ()
      ) else
        let x = if y mod 2 = 1 then x + 1 else x - 1 in
        attempt (i + 1) x (y + 1)
    in
    attempt 0 x y
  in
  loop ()

  (* A general list of resources to add *)
let add_resources_list area =
  match area with
  | EastUS
  | WestUS ->
      [ (Foothills_pixel, CoalMine_pixel, CoalMine, 50);
        (Foothills_pixel, CoalMine_pixel, LumberMill, 100);
        (Clear_pixel,     OilWell_pixel,  OilWell, 10);
      ]
  | England
  | Europe ->
      let count = if area = England then 150 else 50 in
      [ (Foothills_pixel, CoalMine_pixel, CoalMine, count);
        (* NOTE: should be saltmine (Europe) or even farm somehow? *) 
        (Clear_pixel, OilWell_pixel, OilWell, count);
      ]
      
let upgrade_city_pixel = function
  | Clear_pixel
  | Woods_pixel
  | Farm_pixel -> Village_pixel
  | Village_pixel -> City_pixel
  | Desert_pixel -> Farm_pixel
  | x -> x

let add_city_list area city_list : (int * int) list =
  let add_city (factor, acc) (x,y) =
    (* add all cities as villages *)
    let acc = (x,y)::acc in

    (* Determine how many to add *)
    let x_y_func = match area with
      | EastUS ->
          x - (abs(68 - y) / 4) + 128
      | WestUS when x <= 120 ->
          (120 - x) * 16
      | WestUS ->
          x * 8 - 1120
      | England ->
          ((x + y) * 3) / 4 + 32
      | Europe ->
          700 - (x + y) / 3
    in
    let x_y_func = Utils.clip x_y_func 0 767 in
    let max_val = x_y_func / 16 + factor / 4 in
    let n = Random.int max_val |> Utils.clip 0 24 in
    let factor = factor + (x_y_func / 32) - n in

    (* Add extra population around towns *)
    let rec add_population acc _i =
      let offset = Random.int 48 in              (* Use swirl of offsets *)
      let offset = offset - Random.int offset in (* Closer to middle *)
      let x = x + Utils.x_offset.(offset) in
      let y = y + Utils.y_offset.(offset) in
      (x, y)::acc
    in
    let acc = Iter.fold add_population acc Iter.(0 -- (n-1)) in
    (factor, acc)
  in
  List.fold_left add_city (0, []) city_list |> snd

let load_city_list file =
  let filename = "./data/CITIES0.DTA" in
  let str = CCIO.with_in filename CCIO.read_all in
  let stream = Gen.of_string str in
  let rec parse acc _ =
    let x = My_gen.get_word stream in
    let y = My_gen.get_word stream in
    let name = Gen.take 16 stream |> Gen.to_string in
    (* Name is followed by zeros *)
    let name = String.split_on_char (Char.chr 0) name |> List.hd in
    {name; x; y}::acc
  in
  let cities = Iter.fold parse [] Iter.(0 -- 99) |> List.rev in
  List.iter (fun c -> print_endline @@ show_city c) cities


end
