open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers

  type t = {
   (* int: random offset for suffix.
      To make station names interesting, we have a list of suffixes,
      and we distribute random offsets with our cities into that list.
    *)
    map: (int, string * int) Utils.Hashtbl.t;
    arr: (int * int) array; (* locations *)
    width: int;
    height: int;
  } [@@deriving yojson]

  let make map width height =
    let l = Hashtbl.map_list (fun offset _ -> Utils.x_y_of_offset width offset) map in
    let arr = Array.of_list l in
    {map; arr; width; height}

  let iter f v =
    Hashtbl.iter (fun offset city_s ->
      let x, y = Utils.x_y_of_offset v.width offset in
      f x y city_s)
    v.map

  let find_exn v x y = Hashtbl.find v.map (Utils.calc_offset v.width x y)

  let get_name v x y = find_exn v x y |> fst

  let name_of_loc (x,y) v = get_name v x y

  let find v x y = Hashtbl.find_opt v.map (Utils.calc_offset v.width x y)

  let to_list v = CCHashtbl.to_list v.map
    |> List.map (fun (i, (city, _)) ->
      let x, y = Utils.x_y_of_offset v.width i in
      x, y, city)

  let find_close v x y ~range =
    Utils.scan ~range ~x ~y ~width:v.width ~height:v.height
      ~f:(fun x y -> match find v x y with Some _ -> true | None -> false)

  let random random v = Random.pick_array v.arr random

  let random_idx random v = Random.int (Array.length v.arr) random

  let get_idx i v = v.arr.(i)

  let name_by_idx i v =
    let loc = get_idx i v in
    name_by_loc loc v

