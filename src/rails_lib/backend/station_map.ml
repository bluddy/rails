open Containers

include Loc_map

type t = Station.t Loc_map.t
  [@@deriving yojson]

let find_nearest v loc =
  fold (fun (station:Station.t) acc ->
    let station_loc = (station.x, station.y) in
    let dist = Utils.classic_dist loc station_loc in
    match acc with
    | Some (_, min_dist) when dist < min_dist -> Some(station, dist)
    | None -> Some(station, dist)
    | _ -> acc)
  v
  ~init:None
  |> Option.map fst
