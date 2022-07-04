open Containers
open Sexplib.Std

type t = {
  map: (int, Station.t) Hashtbl.t;
  width: int;
} [@@deriving sexp]

let create width =
  let map = Hashtbl.create 10
  in
  {
    map;
    width;
  }

let iter f v =
  Hashtbl.iter (fun offset station ->
    let x, y = Utils.x_y_of_offset v.width offset in
    f x y station)
  v.map

let get v x y = Hashtbl.find_opt v.map (Utils.calc_offset v.width x y)

let add v x y station =
  Hashtbl.replace v.map (Utils.calc_offset v.width x y) station;
  v

let delete v x y =
  Hashtbl.remove v.map (Utils.calc_offset v.width x y);
  v
