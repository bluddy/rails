(* Develop tiles over time *)

open Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let src = Logs.Src.create "tile_develop" ~doc:"Tile_develop"
module Log = (val Logs.src_log src: Logs.LOG)

module C = Constants
module Map = Utils.Map

module PixelMap = Map.Make(struct
  type t = Tilemap.pixel [@@deriving yojson]
  let compare = Tilemap.compare_pixel
end)

type t = {
  (* The contribution of every pixel type to development *)
  develop: int PixelMap.t;
  (* The counterforce to development *)
  resist: int PixelMap.t;
  (* Count of total development, reset every fin period *)
  total_devs: int;
} [@@deriving yojson]

let default_dev =
  [
    Tilemap.Slum_pixel, 16;
    Ocean_pixel, 1;
    Clear_pixel, 2;
    Woods_pixel, 0;
    Harbor_pixel, 12;
    CoalMine_pixel, 0;
    Desert_pixel, 0;
    Foothills_pixel, 0;
    OilWell_pixel, 0;
    River_pixel, 2;
    Farm_pixel, 3;
    Hills_pixel, 0;
    Village_pixel, 8;
    EnemyRR_pixel, 0;
    City_pixel, 16;
    Mountain_pixel, -2;
  ]

let default_resist =
  [
    Tilemap.Slum_pixel, 56;
    Ocean_pixel, 0;
    Clear_pixel, 0;
    Woods_pixel, -3;
    Harbor_pixel, -3;
    CoalMine_pixel, 1;
    Desert_pixel, -3;
    Foothills_pixel, 3;
    OilWell_pixel, 12;
    River_pixel, 3;
    Farm_pixel, 14;
    Hills_pixel, -9;
    Village_pixel, 21;
    EnemyRR_pixel, 3;
    City_pixel, 48;
    Mountain_pixel, -15;
  ]

let default = {
  total_devs = 0;
  develop = PixelMap.of_list default_dev;
  resist = PixelMap.of_list default_resist;
}

(* Natural development of urban growth *)
let _develop_pixel ~difficulty = function
  | Tilemap.Slum_pixel
  | Clear_pixel
  | Farm_pixel -> Tilemap.Village_pixel
  | Woods_pixel -> Clear_pixel
  | Village_pixel -> City_pixel
  | City_pixel when not @@ B_options.easy difficulty -> Slum_pixel
  | x -> x

let _develop_tile ~x ~y tile ~difficulty ~region ~random ~tilemap (v:t) =
  let pixel = Tilemap.pixel_of_tile tile in
  let develop_resource () =
    let clear_pixel = match pixel with
      | Tilemap.CoalMine_pixel -> Tilemap.Hills_pixel
      | _ -> Clear_pixel
    in
    let build = Random.int C.chance_destroy_resource random = 0 in
    if build then
      let x2, y2 = Dir.random_adjust x y random in
      if Tilemap.out_of_bounds x2 y2 tilemap then None, v
      else
        let pixel2 = Tilemap.get_tile tilemap x y |> Tilemap.pixel_of_tile in
        if Tilemap.equal_pixel clear_pixel pixel2 then
          let new_tile = Tilemap.tile_of_pixel ~region ~x ~y ~pixel tilemap in
          if Tile.equal new_tile tile then
            Some (x2, y2, new_tile, false), v
          else None, v
        else None, v
    else (* destroy resource *)
      let clear_tile = Tilemap.tile_of_pixel ~region ~x ~y ~pixel:clear_pixel tilemap in
      Some (x, y, clear_tile, false), v
  in
  let dev_val = PixelMap.get_or pixel v.develop ~default:0 in

  let develop_industry () =
    let pixel2 = _develop_pixel pixel ~difficulty in
    if Tilemap.equal_pixel pixel2 pixel then None, v
    else
      let develop =
        let ocean_val, river_val = if dev_val <= 1 then 1, 2 else dev_val, dev_val in
        v.develop
        |> PixelMap.add Ocean_pixel ocean_val
        |> PixelMap.add River_pixel river_val
      in
      let total_devs = Tilemap.fold_range ~range:1 ~x ~y ~init:0 tilemap
        ~f:(fun acc _ _ tile ->
            let pixel = Tilemap.pixel_of_tile tile in
            let x = PixelMap.get_or pixel v.develop ~default:0 in
            acc + x)
      in
      let resist_dev = PixelMap.get_or pixel v.resist ~default:0 in
      let roll = Random.int (total_devs + 1) random in
      let v = [%up {v with develop}] in
      if roll > resist_dev then
        let dev_val = PixelMap.get_or pixel2 develop ~default:0 in
        let tile2 = Tilemap.tile_of_pixel ~region ~x ~y ~pixel:pixel2 tilemap in
        let go_again = dev_val < 4 in (* Not sure why *)
        Some(x, y, tile2, go_again), v
      else None, v
  in
  if dev_val < 0 then None, v
  else match pixel with
  | Tilemap.CoalMine_pixel
  | OilWell_pixel -> develop_resource ()
  | _ -> develop_industry ()

  (* NOTE: this function in the original has a bunch of dead code around
     the active station, which is always cleared out *)
let develop_tiles ~two_devs ~difficulty ~region ~random ~tilemap ~year
  ~cities ~cities_to_ai ~active_station (v:t) =

  let age_factor = (year - C.reference_year_map_dev) / 16
    |> Utils.clip ~min:3 ~max:9
  in
  let age_factor = if Region.is_west_us region then age_factor / 2 else age_factor in
  let age_factor = age_factor * 2 + 1 in

  let rec loop ~num_devs ~total_devs active_station v =
    if two_devs && num_devs >= 2 || (not two_devs) && num_devs >= 1 then
      {v with total_devs = total_devs}
    else
      let random_add_x_y x y =
        let random_offset () = (Random.int age_factor random) - age_factor in
        x + random_offset (), y + random_offset ()
      in
      let random_tile () =
        let x = (Random.int (Tilemap.get_width tilemap - 2) random) + 1 in
        let y = (Random.int (Tilemap.get_height tilemap - 2) random) + 1 in
        let x =
          (* Add a westward bias *)
          if Region.is_east_us region && year < 1880 then
            let dx = Random.int (x/2) random in
            x - dx
          else x
        in
        x, y
      in
      let x, y = match active_station with
        | Some (x, y) -> random_add_x_y x y
        | None when total_devs land 0xE > 0 ->
          let (x, y) as city = Cities.random random cities in
          if Loc_map.mem city cities_to_ai then
            random_add_x_y x y
          else
            random_tile ()
        | None -> random_tile ()
      in
      let rec wander_loop x y ~num_devs ~total_devs v =
        if Tilemap.out_of_bounds x y tilemap then
          loop ~num_devs ~total_devs None v
        else
          match Tilemap.get_tile tilemap x y with
          | Tile.Ocean _
          | River _ -> loop ~num_devs ~total_devs None v
          | tile ->
              let res, v = _develop_tile ~x ~y tile ~difficulty ~region ~random ~tilemap v in
              match res with
              | Some (x, y, tile, true) ->
                  Log.debug (fun f -> f "Economic development at (%d, %d)" x y);
                  Tilemap.set_tile tilemap x y tile;
                  let x, y = Dir.random_adjust x y random in
                  wander_loop x y ~num_devs:(num_devs + 1) ~total_devs:(total_devs + 1) v
              | Some (x, y, tile, _) ->
                  Log.debug (fun f -> f "Economic development at (%d, %d)" x y);
                  Tilemap.set_tile tilemap x y tile;
                  loop ~num_devs:(num_devs + 1) ~total_devs:(total_devs + 1) None v
              | None ->
                  loop ~num_devs ~total_devs None v
      in
      wander_loop x y ~num_devs ~total_devs v

  in
  loop ~num_devs:0 ~total_devs:v.total_devs active_station v



