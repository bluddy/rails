open Containers
open Utils.Infix

let src = Logs.Src.create "train" ~doc:"Train"
module Log = (val Logs.src_log src: Logs.LOG)

let max_stops = 4

(* A stop of a route *)
type stop = {
  x: int;
  y: int;
  cars: (Goods.t list) option; (* change of cars. None -> "No Change" *)
} [@@deriving yojson]

let make_stop x y cars = {x; y; cars}

type train_type =
  | Local (* Stops at every stop *)
  | Through (* Skips depots *)
  | Express (* Skips stations or less *)
  | Limited (* Skips terminals or less *)
  [@@deriving yojson, enum]

module History = struct
  type elem = {
    x: int;
    y: int;
    dir: Dir.t;
    speed_factor: int;
  } [@@deriving yojson]

  let empty =
    {x=0; y=0; dir=Dir.Up; speed_factor=0}

  type t = {
    history: elem array;
    mutable idx: int;
  } [@@deriving yojson]

  let make () = {
    history=Array.make Constants.train_max_size empty;
    idx=0;
  }

  (* Saves the info about the train over time at midpoints *)
  let add v x y dir speed_factor =
    let hist = {x; y; dir; speed_factor} in
    v.idx <- (succ v.idx) mod Array.length v.history;
    v.history.(v.idx) <- hist;
    ()

  let get v i =
    let idx = (v.idx - i) mod Array.length v.history in
    v.history.(idx)
end

module Car = struct
  type t = {
    good: Goods.t;
    amount: int;
    source: (int * int) option;
  } [@@deriving yojson]

  let make good amount source = { good; amount; source }

  let good v = v.good
end

type t = {
  mutable x: int;
  mutable y: int;
  engine: Engine.t;
  mutable pixels_from_midtile: int;
  mutable dir: Dir.t;
  mutable speed: int; (* x5 to get real speed *)
  mutable wait_time: int; (* for updating train *)
  mutable target_speed: int;
  fiscal_dist_traveled: (int ref * int ref); (* by period. Incremented at mid-tiles *)
  cars: Car.t list;
  freight: Goods.freight; (* freight class *)
  _type: train_type;
  history: History.t; (* History of values. Used for cars *)
  target_stop: int; (* current stop of route *)
  route: stop list; (* route stops *)
  priority: stop option;
} [@@deriving yojson]

let set_target_speed v speed = v.target_speed <- speed  
let set_speed v speed = v.speed <- speed

let get_dest v = 
  let stop = match v.priority with
    | Some stop -> stop
    | None -> List.nth v.route v.target_stop
  in
  (stop.x, stop.y)

let freight_of_cars cars =
  (* Freight goes by the highest level *)
  List.fold_left (fun freight car ->
    let freight2 = Goods.freight_of_goods car in
    if Goods.compare_freight freight2 freight > 0
    then freight2 else freight)
  Goods.FreightMail
  cars

let make (x,y) engine cars other_station ~dir =
  let route = [make_stop x y None] in
  let route = match other_station with
    | Some (x,y) -> route @ [make_stop x y None]
    | None -> route
  in
  let v = {
    x=x * Constants.tile_w;
    y=y * Constants.tile_h;
    engine;
    pixels_from_midtile=0;
    dir;
    speed=1;
    target_speed=1;
    cars=List.map (fun good -> Car.make good 0 None) cars;
    freight=freight_of_cars cars;
    wait_time=0;
    _type=Local;
    history=History.make ();
    target_stop=0;
    route;
    priority=None;
    fiscal_dist_traveled=(ref 0, ref 0);
  }
  in
  Log.debug (fun f -> f "Train: new train at (%d,%d)" x y);
  v

let get_route v = v.route

let remove_stop_car (v:t) stop car =
  let remove_car (stop:stop) =
    let cars = match stop.cars with
      | None -> None
      | Some car_list ->
          begin match List.remove_at_idx car car_list with
          | [] -> None
          | l  -> Some l
          end
    in
    {stop with cars}
  in
  match stop with
  | `Stop stop ->
    let route = Utils.List.modify_at_idx stop remove_car v.route in
    {v with route}
  | `Priority ->
      let priority = Option.map remove_car v.priority in
      {v with priority}

let add_stop_car (v:t) stop car =
  let add_car (stop:stop) =
    let cars = match stop.cars with
      | Some car_list -> Some(car_list @ [car])
      | None -> Some([car])
    in
    {stop with cars}
  in
  match stop with
  | `Stop stop ->
      let route = Utils.List.modify_at_idx stop add_car v.route in
      {v with route}
  | `Priority ->
      let priority = Option.map add_car v.priority in
      {v with priority}

let remove_all_stop_cars (v:t) stop =
  let remove_all_cars (stop:stop) = {stop with cars = Some []} in
  match stop with
  | `Stop stop ->
      let route = Utils.List.modify_at_idx stop remove_all_cars v.route in
      {v with route}
  | `Priority ->
      let priority = Option.map remove_all_cars v.priority in
      {v with priority}

let check_stop_station (v:t) stop (x,y) =
  (* Don't allow setting the station if the previous or next station
     is the same station already *)
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
  match stop with
  | `Stop i ->
    let route =
      (* Check for lengthening *)
      if List.length v.route = i then
        v.route @ [make_stop x y None]
      else
        Utils.List.modify_at_idx i (fun (stop:stop) -> {stop with x; y}) v.route
    in
    {v with route}
  | `Priority ->
      let stop = match v.priority with
        | None -> make_stop x y None
        | Some stop -> {stop with x; y}
      in
      {v with priority=Some stop}

let remove_stop (v:t) stop =
  match stop with
  | `Stop i ->
    let route = List.remove_at_idx i v.route in
    {v with route}

  | `Priority -> {v with priority=None}

let calc_car_pos (v:t) car = ()

(* Cycles used to update integer speed up to index=12
   Note that each step has one more active bit
 *)
let update_cycle_array =
  [| 0; 1; 0x41; 0x111; 0x249; 0x8A5; 0x555; 0x5AD; 0x6DB; 0x777; 0x7DF; 0x7FF; 0xFFF |]

let update_array_length = Array.length update_cycle_array

let update_speed (v:t) ~cycle ~cycle_check ~cycle_bit =
  (* Update current train speed based on target speed and cycle *)
  if v.target_speed >= v.speed then (
    (* accelerate *)
    let speed_diff = min 12 (v.target_speed - v.speed) in
    if v.speed <= 1 ||
       (cycle mod cycle_check = 0 &&
       (update_cycle_array.(speed_diff) land cycle_bit) <> 0) then begin
         v.speed <- succ v.speed;
         Log.debug (fun f -> f "Train accelerate");
    end;
  ) else (
  (* decelerate *)
    if cycle mod 8 = 0 then begin
      v.speed <- pred v.speed;
      Log.debug (fun f -> f "Train accelerate");
    end
  )

let get_weight v =
  List.fold_left (fun weight car ->
    let freight = Goods.freight_of_goods car.Car.good |> Goods.freight_to_enum in
    let weight2 = (car.amount * 4 - 320) / (6 - freight) + 240 in
    weight + weight2)
  0
  v.cars

let get_max_speed_factor v =
  List.foldi (fun speed_factor i _ ->
    (* BUGFIX: i/2 is a weird choice: indexing into history. Maybe to reduce effect of long trains? *)
    let history = History.get v.history i in
    max speed_factor history.History.speed_factor)
  0
  v.cars

let compute_target_speed v ~idx ~cycle =
  let weight = get_weight v in
  let max_speed_factor = get_max_speed_factor v in
  let a = weight / 160 + 1 in
  let b = (max_speed_factor + 2) * a in
  let engine_speed = v.engine.Engine.horsepower * 200 / b in
  let random = (13 * idx + cycle) mod 64 in
  ((engine_speed * 8) + random + 80) / 80
  |> Utils.clip ~min:0 ~max:v.engine.max_speed

let add_dist_traveled v dist period =
  match period, v.fiscal_dist_traveled with
  | `First, (d,_) -> d := !d + dist
  | `Second, (_,d) -> d := !d + dist


