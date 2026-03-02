open Containers

include Loc_map

type t = Station.t Loc_map.t
  [@@deriving yojson]

let find_nearest ~player_idx ?(only_proper=false) loc v =
  (* NOTE: could be made more efficient with quadmap or array *)
  fold (fun (station:Station.t) acc ->
    if not @@ Station.owned_by player_idx station then acc else
    if only_proper && not @@ Station.is_proper_station station then acc else
    let dist = Utils.classic_dist loc @@ Station.get_loc station in
    match acc with
    | Some (_, min_dist) when dist < min_dist -> Some(station, dist)
    | None -> Some(station, dist)
    | _ -> acc)
  v
  ~init:None
  |> Option.map fst

let get_two_proper_stations player_idx v =
  let exception Found of (Utils.loc * Utils.loc) in
  match
    Loc_map.fold_loc (fun loc station acc ->
      if not @@ Station.owned_by player_idx station then acc else
      match acc with
      | x::y::_ -> raise @@ Found(x, y)
      | xs when Station.is_proper_station station -> loc::xs
      | _ -> acc)
    v
    ~init:[]
  with
  | exception Found(x, y) -> Some(x, y)
  | _ -> None

let get_num_proper_stations player_idx v =
  Loc_map.fold (fun station acc ->
    if Station.owned_by player_idx station && Station.is_proper_station station then acc + 1 else acc)
    v
    ~init:0

let clear_priority_shipment_for_all ~players v =
  let open Station in
  Loc_map.map (function
    | station when List.mem ~eq:Owner.equal station.player players ->
        Station.set_priority_shipment false station
    | station -> station)
    v

let have_engine_shop player_idx v =
  find (fun v -> Station.owned_by player_idx v && Station.has_upgrade v Station.EngineShop) v
  |> Option.is_some

let remove_goods goods player_idx v =
  iter (fun station ->
    if Station.owned_by player_idx station then
      Station.remove_goods goods station) v;
  v

let update_signals locus signal v =
  List.fold_left (fun v (loc, upper) ->
    Loc_map.update loc (fun station ->
      Station.set_signal_upper upper signal station)
    v)
  v
  locus

let num_stations_of_city (x, y) player_idx v =
  fold (fun station count ->
    if not @@ Station.owned_by player_idx station then count else
    match Station.get_city station with
    | Some (city_x, city_y) ->
      if city_x = x && city_y = y then count + 1
      else count
    | _ -> count)
  v
  ~init:0

