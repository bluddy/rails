open Containers
open Utils.Infix

let max_stops = 4

(* A stop of a route *)
type stop = {
  x: int;
  y: int;
  cars: (Goods.t list) option; (* change of cars. None -> "No Change" *)
} [@@deriving yojson]

let make_stop x y cars = {x; y; cars}

type t = {
  x: int;
  y: int;
  engine: Engine.t;
  dir: Dir.t;
  speed: int;
  target_speed: int;
  cars: (Goods.t * int) list; (* good type, amount to 160, /4 = tons *)
  freight: Goods.freight; (* freight class *)

  target_stop: int; (* current stop of route *)
  route: stop list; (* route stops *)
  priority: stop option;
} [@@deriving yojson]

let freight_of_cars cars =
  (* Freight goes by the highest level *)
  List.fold_left (fun freight car ->
    let freight2 = Goods.freight_of_goods car in
    if Goods.compare_freight freight2 freight > 0
    then freight2 else freight)
  Goods.FreightMail
  cars

let make x y engine cars =
  {
    x; y;
    engine;
    dir=Dir.Up;
    speed=0;
    target_speed=0;
    cars=List.map (fun x -> (x,0)) cars;
    freight=freight_of_cars cars;
    target_stop=0;
    route=[make_stop x y None];
    priority=None;
  }

let get_route v = v.route

let remove_stop_car (v:t) stop car =
  let route =
    Utils.List.modify_at_idx stop (fun (stop:stop) ->
    let cars = match stop.cars with
      | None -> None
      | Some car_list -> Some(List.remove_at_idx car car_list)
    in
    {stop with cars})
    v.route
  in
  {v with route}

let add_stop_car (v:t) stop car =
  let route =
    Utils.List.modify_at_idx stop (fun (stop:stop) ->
    let cars = match stop.cars with
      | Some car_list -> Some(car_list @ [car])
      | None -> Some([car])
    in
    {stop with cars})
    v.route
  in
  {v with route}

let remove_all_stop_cars (v:t) stop =
  let route =
    Utils.List.modify_at_idx stop
      (fun (stop:stop) -> {stop with cars=None})
      v.route
  in
  {v with route}

let check_stop_station (v:t) stop (x,y) =
  (* Don't allow setting the station if the previous or next station is the same station already *)
  let len = List.length v.route in
  let prev, next = match stop with
    | 0 when len >= 2 -> None, Some 1
    | 0 -> None, None
    | s when len >= (s + 1) -> Some (s-1), Some (s+1)
    | s -> Some (s-1), None
  in
  let check = function
    | Some i ->
        let stop = List.nth v.route i in
        Utils.eq_xy x y stop.x stop.y
    | None -> false
  in
  not (check prev || check next)

let set_stop_station (v:t) stop (x,y) =
  let route =
    (* Check for lengthening *)
    if List.length v.route = stop then
      v.route @ [make_stop x y None]
    else
      Utils.List.modify_at_idx stop (fun (stop:stop) -> {stop with x; y}) v.route
  in
  {v with route}


