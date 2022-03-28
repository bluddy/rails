module Ndarray = Owl_base_dense_ndarray.Generic

type area =
  | EastUS
  | WestUS
  | England
  | Europe

type tile =
  | Clear
  | Woods
  | Swamp
  | Foothills
  | Hills
  | Mountains
  | City
  | Village
  | Farm
  | Slums
  | FoodProc
  | Ranch
  | Stockyard
  | Factory
  | GrainElev
  | PaperMill
  | Landing
  | LumberMill
  | CoalMine
  | SteelMill
  | PowerPlant
  | OilWell
  | SaltMine
  | Refinery
  | EnemyRR
  | River
  | Ocean
  | Harbor

  | Desert

type t = tile array

type pixel =
  | Slum_pixel
  | Ocean_pixel
  | Clear_pixel
  | Woods_pixel
  | Harbor_pixel
  | CoalMine_pixel
  | Desert_pixel
  | Foothills_pixel
  | OilWell_pixel
  | River_pixel
  | Farm_pixel
  | Hills_pixel
  | Village_pixel
  | EnemyRR_pixel
  | City_pixel
  | Mountain_pixel
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

let offset x y = y * map_width + x
let get_tile map x y = map.(offset x y)

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

  (* Create a list of random mountains to add to the map, based on area *)
let add_all_mountains area =
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
        let x = (* Bring closer to 0 *)
          if x >= 0 then (* check eq *)
            x - Random.int 100
          else
            x + Random.int 100;
        in
        let a, b = match area with
          | EastUS -> (x * 200, 400 - y)
          | WestUS -> (x * 32, 200 - formula)
          | _ -> assert false
        in
        let x = a / b + formula in
        let x, y =
          if y < 80 then
            let y = y - Random.int 35 in
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
              let y = y + 38 in
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

