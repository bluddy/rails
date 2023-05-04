open Containers
open Utils.Infix

let src = Logs.Src.create "train" ~doc:"Train"
module Log = (val Logs.src_log src: Logs.LOG)

module Vector = Utils.Vector
module Hashtbl = Utils.Hashtbl
module C = Constants

let max_stops = 4

(* A stop of a route *)
type stop = {
  x: int;
  y: int;
  cars: (Goods.t list) option; (* change of cars. None -> "No Change" *)
} [@@deriving yojson, show]

let make_stop x y cars = {x; y; cars}

type train_type =
  | Local (* Stops at every stop *)
  | Through (* Skips depots *)
  | Express (* Skips stations or less *)
  | Limited (* Skips terminals or less *)
  [@@deriving yojson, enum, show]

module History = struct
  type elem = {
    x: int;
    y: int;
    dir: Dir.t;
    speed_factor: int;
  } [@@deriving yojson, show]

  let empty =
    {x=0; y=0; dir=Dir.Up; speed_factor=0}

  type t = {
    history: elem array;
    mutable idx: int;
  } [@@deriving yojson, show]

  let make () = {
    history=Array.make C.train_max_size empty;
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
  type load = {
    amount: int; (* 160 is full (40 tons) *)
    source: Station.id;
    cycle: int;
  } [@@ deriving yojson, show]

  type t = {
    good: Goods.t;
    load: load option;
  } [@@deriving yojson, show]

  let make good = {good; load=None}
  let get_good v = v.good
  let get_freight v = Goods.freight_of_goods @@ get_good v
  let get_age v cycle = match v.load with
    | Some load -> cycle - load.cycle
    | None -> assert false
  let get_source v = match v.load with
    | Some {source;_} -> source
    | None -> assert false
  let get_amount v = match v.load with
    | Some load -> load.amount
    | None -> 0

  let empty v = {v with load=None}

end

type t = {
  mutable x: int;
  mutable y: int;
  mutable pixels_from_midtile: int;
  mutable dir: Dir.t;
  mutable speed: int; (* *5 to get real speed *)
  mutable target_speed: int;
  mutable wait_time: int; (* for loading/unloading train *)
  segment: Segment.id option; (* for track semaphores *)
  name: string option;
  last_station: Station.id;
  stop_at_station: bool;
  station_state: [`Traveling | `Entered ];
  engine: Engine.t;
  cars: Car.t list;
  freight: Goods.freight; (* freight class *)
  _type: train_type;
  history: History.t; (* History of values. Used for cars *)
  stop: int; (* current stop of route *)
  route: stop Utils.Vector.vector; (* route stops *)
  priority: stop option;
  had_maintenance: bool;
  dist_traveled: (int ref * int ref); (* by period. Incremented at mid-tiles *)
  dist_shipped_cargo: int * int; (* also by fin period *)
} [@@deriving yojson, show]

let set_target_speed v speed = v.target_speed <- speed  
let set_speed v speed = v.speed <- speed

let get_route_length v = Vector.length v.route
let get_route_stop v i = Vector.get v.route i

let get_route_dest v = Vector.get v.route v.stop
let get_stop v =
  match v.priority with
  | Some stop -> stop
  | None -> get_route_dest v
let get_dest v = 
  let stop = get_stop v in
  (stop.x, stop.y)

let freight_of_cars cars =
  (* Freight goes by the highest level *)
  List.fold_left (fun freight car ->
    let freight2 = Car.get_freight car in
    if Goods.compare_freight freight2 freight > 0
    then freight2 else freight)
  Goods.FreightMail
  cars

let get_car_goods_count cars =
  let h = Hashtbl.create 10 in
  List.iter (fun car -> Hashtbl.incr ~by:1 h car.Car.good) cars;
  h

let make ((x,y) as station) engine cars other_station ~dir =
  let route = [make_stop x y None] in
  let route = match other_station with
    | Some (x,y) -> [make_stop x y None] @ route
    | None -> route
  in
  let route = Vector.of_list route in
  let cars = List.map Car.make cars in
  let v = {
    x=x * C.tile_w + C.tile_w / 2;
    y=y * C.tile_h + C.tile_h / 2;
    engine;
    pixels_from_midtile=0;
    dir;
    speed=1;
    target_speed=1;
    cars;
    freight=freight_of_cars cars;
    wait_time=0;
    _type=Local;
    history=History.make ();
    stop=0;
    route;
    segment=None;
    name=None;
    last_station=station;
    had_maintenance=false;
    stop_at_station=false;
    station_state=`Traveling;
    priority=None;
    dist_traveled=(ref 0, ref 0);
    dist_shipped_cargo=(0, 0);
  }
  in
  Log.debug (fun f -> f "Train: new train at (%d,%d) speed:%d targer_speed:%d" v.x v.y v.speed v.target_speed);
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
  | `Stop stop_idx ->
      Vector.modify_at_idx v.route stop_idx remove_car;
      v
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
  | `Stop stop_idx ->
      Vector.modify_at_idx v.route stop_idx add_car;
      v
  | `Priority ->
      let priority = Option.map add_car v.priority in
      {v with priority}

let remove_all_stop_cars (v:t) stop =
  let remove_all_cars (stop:stop) = {stop with cars = Some []} in
  match stop with
  | `Stop stop_idx ->
      Vector.modify_at_idx v.route stop_idx remove_all_cars;
      v
  | `Priority ->
      let priority = Option.map remove_all_cars v.priority in
      {v with priority}

let check_stop_station (v:t) stop loc =
  (* Don't allow setting the station if the previous or next station
     is the same station already *)
  let len = Vector.length v.route in
  let prev, next = match stop with
    | 0 when len >= 2 -> None, Some 1
    | 0 -> None, None
    | s when len >= (s + 1) -> Some (s-1), Some (s+1)
    | s -> Some (s-1), None
  in
  let check = function
    | Some i ->
        let stop = Vector.get v.route i in
        Utils.eq_xy loc (stop.x,stop.y)
    | None -> false
  in
  not (check prev || check next)

let set_stop_station (v:t) stop (x,y) =
  match stop with
  | `Stop i ->
      (* Check for lengthening *)
      if (Vector.length v.route) = i then begin
        Vector.push v.route (make_stop x y None)
      end else begin
        Vector.modify_at_idx v.route i (fun (stop:stop) -> {stop with x; y})
      end;
      v
  | `Priority ->
      let stop = match v.priority with
        | None -> make_stop x y None
        | Some stop -> {stop with x; y}
      in
      {v with priority=Some stop}

let remove_stop (v:t) stop =
  match stop with
  | `Stop i ->
      Vector.remove_and_shift v.route i;
      v
  | `Priority -> {v with priority=None}

(* Cycles used to update integer speed up to index=12
   Note that each step has one more active bit
 *)
let update_cycle_array =
  [| 0; 1; 0x41; 0x111; 0x249; 0x8A5; 0x555; 0x5AD; 0x6DB; 0x777; 0x7DF; 0x7FF; 0xFFF |]

let update_array_length = Array.length update_cycle_array

let update_speed (v:t) ~cycle ~cycle_check ~cycle_bit =
  (* Update current train speed based on target speed and cycle *)
  if v.target_speed > v.speed then (
    (* accelerate *)
    let speed_diff = min 12 (v.target_speed - v.speed) in
    if v.speed <= 1 ||
       (cycle mod cycle_check = 0 &&
       (update_cycle_array.(speed_diff) land cycle_bit) <> 0) then begin
         v.speed <- succ v.speed;
         Log.debug (fun f -> f "Train accelerate. New speed %d" v.speed);
    end;
  ) else if v.target_speed < v.speed then (
  (* decelerate *)
    if cycle mod 8 = 0 then begin
      v.speed <- pred v.speed;
      Log.debug (fun f -> f "Train decelerate. New speed %d" v.speed);
    end
  );
  v

let get_weight v =
  List.fold_left (fun weight car ->
    let freight = Goods.freight_of_goods car.Car.good |> Goods.freight_to_enum in
    let weight2 = (Car.get_amount car * 4 - 320) / (6 - freight) + 240 in
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
  match period, v.dist_traveled with
  | `First, (d,_) -> d := !d + dist
  | `Second, (_,d) -> d := !d + dist

let add_dist_shipped_cargo v dist period =
  match period, v.dist_shipped_cargo with
  | `First, (d,x) -> (d + dist, x)
  | `Second, (x,d) -> (x, d + dist)

let advance (v:t) =
  (* Always advance train by single pixel *)
  let dx, dy = Dir.to_offsets v.dir in
  v.x <- v.x + dx;
  v.y <- v.y + dy;
  v.pixels_from_midtile <- succ v.pixels_from_midtile;
  Log.debug (fun f -> f "Train at (%d, %d)" v.x v.y);
  v

let check_increment_stop v (x,y) =
  match v.priority, Vector.get v.route v.stop with
  | Some stop, _ when stop.x = x && stop.y = y ->
      (* When exiting priority mode, we go to the closest station *)
      let min_stop_idx =
        Vector.foldi (fun i ((min_dist,_) as acc) (stop:stop) ->
          let dist = Utils.classic_dist (stop.x, stop.y) (x,y) in
          if dist < min_dist then (dist, i) else acc)
          (9999, 0)
          (v.route)
        |> snd
      in
      None, min_stop_idx
  | _, stop when stop.x = x && stop.y = y ->
      None, (v.stop + 1) mod (Vector.length v.route)
  | _ ->
      v.priority, v.stop

let calc_arrival_money ~loc ~train ~car ~rates ~region ~west_us_route_done ~difficulty ~year ~year_start ~cycle =
  let mult = if Region.is_us region then 1 else 2 in
  let calc_dist =
    Utils.classic_dist loc (Car.get_source car) * mult
  in
  let car_age = Car.get_age car cycle in
  let reward = calc_dist * 16 / (car_age / 24) in
  let calc_dist =
    if Region.is_west_us region && not west_us_route_done then
      let dx, dy = Utils.dxdy train.last_station loc in
      dx * 3 / 2 + dy / 2
    else calc_dist
  in
  let freight = Goods.freight_to_enum @@ Car.get_freight car in
  let v1 = (calc_dist * (5 - freight) + 40 * freight + 40) / 8 in
  let fp2 = freight * freight * 2 in
  let v3 = (year - 1790) / 10 + fp2 in
  let v6 = (Car.get_amount car)/2 * (reward + fp2) * v1 / v3 in
  let v12 = v6 / (year - year_start/3 - 1170) in
  let money = (7 - B_options.difficulty_to_enum difficulty) * v12 / 3 in
  let money = match rates with
    | `Normal -> money
    | `Double -> 2 * money
    | `Half -> money / 2
  in
  let money = Utils.clip money ~min:2 ~max:9999 in 
  let money = match car.good with
    | Goods.Mail -> money * 5 / 4
    | Passengers -> money * 3 / 2
    | _ -> money
  in 
  money / 2 (* Seems like it's divided in the end *)

let dump_unused_cars_to_station (v:t) station_supply =
  (* dump unused goods at the station at this stage *)
  (* return time for changing cars *)
  (* TODO: clear priority route cars *)
  let stop = get_stop v in
  match stop.cars with
  | None -> (* No adjustment *)
      0, 0, v.cars, station_supply
  | Some stop_cars ->
      let train_cars_by_good = 
        let h = Hashtbl.create 10 in
        List.iter (fun car -> Hashtbl.add_list h (Car.get_good car) car)
        v.cars;
        h
      in
      (* Create the new cars for the train using old cars if possible *)
      let added_cars, train_cars =
        List.fold_map (fun cnt good ->
          match Hashtbl.get train_cars_by_good good with
          | Some (car::cars) ->
              Hashtbl.replace train_cars_by_good good cars;
              cnt, car
          | _ ->
              let car = Car.make good in
              cnt + 1, car)
        0
        stop_cars
      in
      (* Remaining cars are added to the station supply *)
      let removed_cars =
        Hashtbl.fold (fun good cars cnt ->
          List.fold_left (fun cnt car ->
            Hashtbl.incr ~by:(Car.get_amount car) station_supply good;
            cnt + 1)
          cnt
          cars)
        train_cars_by_good
        0
      in
      let expense = (removed_cars - added_cars) * C.car_cost in
      let work_done = removed_cars + added_cars in
      work_done, expense, train_cars, station_supply
                           
let fill_train_from_station cars source cycle station_supply =
  let pickup_amount = 
    List.map (fun car ->
      let car_amount, good = Car.get_amount car, Car.get_good car in
      let station_amount = Hashtbl.get_or station_supply good ~default:0 in
      Utils.clip station_amount ~min:0 ~max:(C.car_amount - car_amount)
    )
    cars
  in
  let cars =
    List.map2 (fun (car:Car.t) add_amount ->
      match car.load with
      | Some load ->
        let amount = load.amount + add_amount in
        if load.amount < add_amount then
          let load = Some {Car.amount; source; cycle} in
          {car with load}
        else
          {car with load=Some {load with amount}}
      | None ->
          {car with load=Some {amount=add_amount; source; cycle}})
    cars
    pickup_amount
  in
  let time_pickup =
    List.fold_left2 (fun time amt car ->
      let freight = Car.get_freight car |> Goods.freight_to_enum in
      time + (amt * freight / 32))
      0
      pickup_amount
      cars
  in
  let station_supply =
    List.iter2 (fun car amount ->
      Hashtbl.decr station_supply (Car.get_good car) ~by:amount)
    cars
    pickup_amount
  in
  time_pickup, cars, station_supply

