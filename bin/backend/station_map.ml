open Containers

let find_nearest v loc =
  Loc_map.fold (fun (station:Station.t) acc ->
    let station_loc = (station.x, station.y) in
    let dist = Utils.classic_dist loc station_loc in
    match acc with
    | Some (_, min_dist) when dist < min_dist -> Some(station, dist)
    | None -> Some(station, dist)
    | _ -> acc)
  v
  ~init:None
  |> Option.map fst
