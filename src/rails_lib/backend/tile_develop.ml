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
  development: int PixelMap.t;
  (* The counterforce to development *)
  block: int PixelMap.t;
}

let develop_tile ~x ~y tile random tilemap v =
  let pixel = Tilemap.pixel_of_tile tile in
  let develop_resource pixel =
    let clear_pixel = match pixel with
      | CoalMine_pixel -> Hills_pixel
      | _ -> Clear_pixel
    in
    let build = Random.int C.chance_destroy_resource random = 0 in
    if build then
      let dir = Dir.random random in
      let x, y = Dir.adjust dir x y in
      let pixel2 = Tilemap.get_tile tilemap x y in


    else (* destroy resource *)
      (x, y, clear_pixel)
  in
  let dev_val = PixelMap.get_or pixel v.development ~default:0 in
  match pixel with
  | _ when dev_val < 0 -> pixel, v
  | CoalMine_pixel
  | Oilwell_pixel -> develop_resource ()
  | Slum_pixel
  | Clear_pixel
  | Farm_pixel
  | Village_pixel
  | City_pixel -> develop_industry ()
  | _ -> pixel, v

