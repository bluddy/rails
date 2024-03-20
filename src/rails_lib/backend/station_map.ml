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

let get_two_proper_stations v =
  let exception Found of (Utils.loc * Utils.loc) in
  match
    Loc_map.fold_loc (fun loc station acc ->
      match acc with
      | x::y::_ -> raise @@ Found(x, y)
      | xs when Station.is_proper_station station -> loc::xs
      | _ -> acc)
    v
    ~init:[]
  with
  | exception Found(x, y) -> Some(x, y)
  | _ -> None
  
