open Containers

(* A common pattern: a hashtbl from location to a thing *)

type 'a t = {
  map: (int, 'a) Utils.Hashtbl.t;
  width: int;
} [@@deriving yojson]

let create width =
  let map = Hashtbl.create 10
  in
  {
    map;
    width;
  }

let length v =
  Hashtbl.length v.map

let iter f v =
  Hashtbl.iter (fun _ station -> f station) v.map

let fold f v ~init =
  Hashtbl.fold (fun _ station acc -> f station acc) v.map init

let get v x y = Hashtbl.find_opt v.map (Utils.calc_offset v.width x y)

let get_exn v x y = Hashtbl.find v.map (Utils.calc_offset v.width x y)

let add v x y station =
  Hashtbl.replace v.map (Utils.calc_offset v.width x y) station;
  v

let delete v x y =
  Hashtbl.remove v.map (Utils.calc_offset v.width x y);
  v

let filter v f =
  CCHashtbl.to_iter v.map |> Iter.filter (fun (_,station) -> f station)
  |> Iter.map (fun (_,station) -> station)

