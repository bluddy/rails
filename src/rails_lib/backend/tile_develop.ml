(* Develop tiles over time *)

open Containers

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

let develop_tile ~x ~y tile ~difficulty ~region ~random ~tilemap (v:t) =
  let pixel = Tilemap.pixel_of_tile tile in
  let develop_resource () =
    let clear_pixel = match pixel with
      | Tilemap.CoalMine_pixel -> Tilemap.Hills_pixel
      | _ -> Clear_pixel
    in
    let build = Random.int C.chance_destroy_resource random = 0 in
    if build then
      let dir = Dir.random random in
      let x2, y2 = Dir.adjust dir x y in
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
      let total_dev = Tilemap.fold_range ~range:1 ~x ~y ~init:0 tilemap
        ~f:(fun acc _ _ tile ->
            let pixel = Tilemap.pixel_of_tile tile in
            let x = PixelMap.get_or pixel v.develop ~default:0 in
            acc + x)
      in
      let resist_dev = PixelMap.get_or pixel v.resist ~default:0 in
      let roll = Random.int (total_dev + 1) random in
      let v = [%up {v with develop}] in
      if roll > resist_dev then
        let dev_val = PixelMap.get_or pixel2 develop ~default:0 in
        let tile2 = Tilemap.tile_of_pixel ~region ~x ~y ~pixel:pixel2 tilemap in
        let _done = dev_val < 4 in (* Not sure why *)
        Some(x, y, tile2, _done), v
      else None, v
  in
  if dev_val < 0 then None, v
  else match pixel with
  | Tilemap.CoalMine_pixel
  | OilWell_pixel -> develop_resource ()
  | _ -> develop_industry ()

