open Containers
open Gmap

let add_mountain_pixel = function
  | Foothills_pixel -> Hills_pixel
  | Hills_pixel -> Mountain_pixel
  | Ocean_pixel
  | River_pixel
  | Harbor_pixel as x -> x
  | _ -> Hills_pixel

  (* Create a list of random mountains to add to the map, based on area *)
let add_mountains_list r area =
  (* Standard mountains *)
  let rec standard_range_loop j acc =
    if j >= 384 then acc
    else
      let x = Random.int 250 r + 1 in (* 1 - 250 *)
      let y = Random.int 186 r + 6 in (* 6 - 191 *)
      let n = Random.int  4 r + 2 in (* 2 - 5 *)

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
              if Random.int 4 r = 0 then x + 1 else x
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

        let y = Random.int 133 r + 35 in (* 35 - 167 *)
        let y =
          if y > 150 then y + Random.int 20 r else y
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
        let x = Random.int 200 r - 100 in (* -100 to 99 *)
        let delta_x = Random.int 100 r in
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
            let y = y - Random.int 35 r in (* move up *)
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
          let n = Random.int 16 r + 3 in
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
                if Random.int max r = 0 then
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
     be in certain tiles, it may fail and need to try again and again.
   *)
let add_resource area ~map ~land_type ~resource_pixel ~wanted_tile ~random_seed ~r =
  let rec loop () =
    let x = Random.int 256 r in
    let y = Random.int 192 r in
    let x = match area with
      | EastUS -> x + Random.int (319 - x) r 
      | WestUS when x >= 120 -> x + Random.int (255 - x) r
      | WestUS -> x - Random.int x r
      | Britain -> x + Random.int (160 - x) r
      | Europe -> x
    in
    let rec attempt i x y =
      if i >= 2 then false else
      let offset = calc_offset x y in
      let tile = map.(offset) in
      let possible_tile = tile_of_pixel ~x ~y ~random_seed ~pixel:resource_pixel in
      if Gmap.equal_tile tile land_type && Gmap.equal_tile possible_tile wanted_tile then (
        map.(offset) <- possible_tile;
        true
      ) else
        let x = if y mod 2 = 1 then x + 1 else x - 1 in
        attempt (i + 1) x (y + 1)
    in
    if attempt 0 x y then ()
    else loop ()   (* Keep trying *)
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
  | Britain
  | Europe ->
      let count = if Gmap.equal_area area Britain then 150 else 50 in
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

let add_city_list r area city_list : (int * int) list =
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
      | Britain ->
          ((x + y) * 3) / 4 + 32
      | Europe ->
          700 - (x + y) / 3
    in
    let x_y_func = Utils.clip x_y_func 0 767 in
    let max_val = x_y_func / 16 + factor / 4 in
    let n = Random.int max_val r |> Utils.clip 0 24 in
    let factor = factor + (x_y_func / 32) - n in

    (* Add extra population around towns *)
    let add_population acc _i =
      let offset = Random.int 48 r in              (* Use swirl of offsets *)
      let offset = offset - Random.int offset r in (* Closer to middle *)
      let x = x + Utils.x_offset.(offset) in
      let y = y + Utils.y_offset.(offset) in
      (x, y)::acc
    in
    let acc = Iter.fold add_population acc Iter.(0 -- (n-1)) in
    (factor, acc)
  in
  List.fold_left add_city (0, []) city_list |> snd

let load_city_list ?(debug=false) area  =
  let num = Gmap.area_to_enum area in
  let filename = Printf.sprintf "./data/CITIES%d.DTA" num in
  let str = CCIO.with_in filename CCIO.read_all in
  let stream = Gen.of_string str in
  let parse acc _ =
    let x = My_gen.get_word stream in
    let y = My_gen.get_word stream in
    let name = Gen.take 16 stream |> Gen.to_string in
    (* Name is followed by zeros *)
    let name = String.split_on_char (Char.chr 0) name |> List.hd in
    {name; x; y}::acc
  in
  let cities = Iter.fold parse [] Iter.(0 -- 99) |> List.rev in
  if debug then
    List.iter (fun c -> print_endline @@ show_city c) cities;
  cities

