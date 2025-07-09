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

  let default = make (Hashtbl.create 10) 0 0

  let iter f v =
    Hashtbl.iter (fun offset city_s ->
      let x, y = Utils.x_y_of_offset v.width offset in
      f x y city_s)
    v.map

  let find_exn x y v = Hashtbl.find v.map (Utils.calc_offset v.width x y)

  let get_name x y v = find_exn x y v |> fst

  let name_of_loc (x,y) v = get_name x y v

  let find x y v = Hashtbl.find_opt v.map (Utils.calc_offset v.width x y)

  let to_list v = CCHashtbl.to_list v.map
    |> List.map (fun (i, (city, _)) ->
      let x, y = Utils.x_y_of_offset v.width i in
      x, y, city)

  let find_close_xy x y ~range v =
    Utils.scan x y ~range ~width:v.width ~height:v.height
      ~f:(fun x y -> match find x y v with Some _ -> true | None -> false)

  let find_close (x,y) ~range v = find_close_xy x y ~range v

  let random random v = Random.pick_array v.arr random

