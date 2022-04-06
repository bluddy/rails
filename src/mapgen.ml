open Containers
open Gmap

let debug = false

let pixel_apply_mountain = function
  | Foothills_pixel -> Hills_pixel
  | Hills_pixel -> Mountain_pixel
  | Ocean_pixel
  | River_pixel
  | Harbor_pixel as x -> x
  | _ -> Foothills_pixel

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
  let mountains = match area with
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
  in
  mountains |> List.rev

  (* We loop until we manage to add the given resource
     This particular function needs to access the map, and since resources can only 
     be in certain tiles, it may fail and need to try again and again.
   *)
let add_resource area ~map ~land_pixel ~resource_pixel ~wanted_tile ~r =
  let rec loop () =
    let x = Random.int 256 r in
    let y = Random.int 192 r in
    let x = match area with
      | EastUS -> x + Random.int (319 - x) r 
      | WestUS when x >= 120 -> x + Utils.random_int (255 - x) r
      | WestUS -> if x > 0 then x - Random.int x r else 0
      | Britain -> x + Random.int (160 - x) r
      | Europe -> x
    in
    let rec attempt i x y =
      (* In the game, we just intrude into the black border and nothing happens.
         Here we have to test *)
      if i >= 2 || x < 0 || y < 0 || x >= 256 || y >= 192 then None else 
      let pixel = Gmap.get_pixel ~map ~x ~y in
      let possible_tile = tile_of_pixel ~x ~y ~pixel:resource_pixel ~map in
      if Gmap.equal_pixel pixel land_pixel && Gmap.equal_tile possible_tile wanted_tile then (
        Gmap.set_pixel ~map ~x ~y ~pixel:resource_pixel;
        Some (x, y)
      ) else
        let x = if y mod 2 = 1 then x + 1 else x - 1 in
        attempt (i + 1) x (y + 1)
    in
    match attempt 0 x y with
    | None -> loop ()
    | x -> x
  in
  loop () |> Option.get_exn_or "Impossible failure reached"

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
      
let pixel_apply_city = function
  | Clear_pixel
  | Woods_pixel
  | Farm_pixel -> Village_pixel
  | Village_pixel -> City_pixel
  | Desert_pixel -> Farm_pixel
  | x -> x

let add_city_list r area (city_list:city list) : (int * int) list =
  let add_city (factor, acc) {x;y;_} =
    (* add all cities as villages *)
    let acc =
      (* Amazingly, some cities seem to be out of bounds and they let the population expand into the map *)
      if x >= 0 && y >= 0 && x < Gmap.map_width && y < Gmap.map_height then
        (x,y)::acc
      else acc
    in

    (* Determine how many to add *)
    let x_y_func = match area with
      | EastUS ->
          x - (abs(68 - y) / 4) + 128
      | WestUS when x <= 120 ->
          (120 - x) * 16
      | WestUS -> (* x > 120 *)
          x * 8 - 1120 (* at least 1600 - 1120 *)
      | Britain ->
          ((x + y) * 3) / 4 + 32
      | Europe ->
          700 - (x + y) / 3
    in
    let x_y_func = Utils.clip x_y_func ~min:0 ~max:767 in
    let max_val = x_y_func / 16 + factor / 4 in
    (* n is 0 to 24 *)
    let n = Random.int max_val r |> Utils.clip ~min:0 ~max:24 in
    (* x_y_func is at most 767. /32 = 23 *)
    let factor = factor + (x_y_func / 32) - n in
    if debug then
      Printf.printf "x[%d] y[%d] x_y_func[%d] max_val[%d] n[%d] factor[%d]\n" x y x_y_func max_val n factor;

    (* Add extra population around towns *)
    let add_population acc _i =
      let offset = Random.int 48 r in              (* Use swirl of offsets *)
      let offset = if offset > 0 then offset - Random.int offset r else 0 in (* Closer to middle *)
      let x = x + Utils.x_offset.(offset) in
      let y = y + Utils.y_offset.(offset) in
      (* Here too, the game relies on borders and we need bounds checking *)
      if x >= 0 && y >= 0 && x < Gmap.map_width && y < Gmap.map_height then
        (x, y)::acc
      else
        acc
    in
    let acc = Iter.fold add_population acc Iter.(0 -- (n-1)) in
    (factor, acc)
  in
  List.fold_left add_city (0, []) city_list |> snd |> List.rev


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

module IntIntMap = Map.Make(struct type t = int * int let compare = Stdlib.compare end)

type t = {
  area: area;
  mountains : (int * int) list;
  resources: (pixel * pixel * tile * int) list;
  cities: (int * int) list;
  state: [`Mountains | `Resources | `Cities | `Done];
  new_pixels: pixel IntIntMap.t;
}

let init r area cities =
  let mountains = add_mountains_list r area in
  let resources = add_resources_list area in
  let cities = add_city_list r area cities in
  let state = `Mountains in
  let new_pixels = IntIntMap.empty in
  {area; mountains; resources; cities; state; new_pixels}

  (* Perform a step of updating the map *)
let update_map_step r v (map:Gmap.t) =
  match v.state with
  | `Mountains -> 
      begin match v.mountains with
      | (x, y)::rest ->
          let pixel = Gmap.get_pixel ~map ~x ~y in
          let pixel = pixel_apply_mountain pixel in
          Gmap.set_pixel ~map ~x ~y ~pixel;
          let new_pixels = IntIntMap.add (x, y) pixel v.new_pixels in
          {v with mountains=rest; new_pixels}
      | _ ->
          {v with state=`Resources}
      end
  | `Resources ->
      begin match v.resources with
      | (_, _, _, 0)::rest ->
          {v with resources=rest}
      | (land_pixel, resource_pixel, wanted_tile, num)::rest ->
          let x, y =
            add_resource v.area ~map ~land_pixel ~resource_pixel ~wanted_tile ~r
          in
          let new_pixels = IntIntMap.add (x, y) resource_pixel v.new_pixels in
          let resources = (land_pixel, resource_pixel, wanted_tile, num-1)::rest in
          {v with resources; new_pixels}
      | _ -> {v with state=`Cities}
      end
  | `Cities ->
      begin match v.cities with
      | (x, y)::rest ->
          Printf.printf "x[%d] y[%d]\n" x y;
          let pixel = Gmap.get_pixel ~map ~x ~y in
          let pixel = pixel_apply_city pixel in
          Gmap.set_pixel ~map ~x ~y ~pixel;
          let new_pixels = IntIntMap.add (x, y) pixel v.new_pixels in
          {v with cities=rest; new_pixels}
      | _ ->
          {v with state = `Done}
      end
  | `Done -> v

module R = Renderer

let render_new_pixels win v pixel_tex =
  let render (x, y) pixel _acc =
    let color = Gmap.pixel_to_enum pixel |> Ega.get_rgb in
    R.render win ~x ~y ~color pixel_tex
  in
  IntIntMap.fold render v.new_pixels (Result.return ())

