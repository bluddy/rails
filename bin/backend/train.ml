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
  [@@deriving yojson, enum, show {with_path = false}]

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
    history=Array.make (C.train_max_size + 1) empty;
    idx=0;
  }

  (* Saves the info about the train over time at midpoints *)
  let add v x y dir speed_factor =
    let hist = {x; y; dir; speed_factor} in
    v.idx <- (succ v.idx) mod Array.length v.history;
    v.history.(v.idx) <- hist;
    (* Log.debug (fun f -> f "Add: %s" (show v)); *)
    ()

  let get v i =
    let idx = Utils.modulo (v.idx - i) @@ Array.length v.history in
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

type state =
  | Traveling of { mutable speed: int; (* *5 to get real speed *)
                   mutable target_speed: int;
                   last_stop: int * int; (* To prevent double processing *)
                }
  | WaitingAtStation of {mutable wait_time: int}
  [@@deriving yojson, show]

  (* TODO: 
    other states:
      holding
      Waiting for full load
      waiting at siding
      stopped at signal
    *)

type t = {
  mutable x: int;
  mutable y: int;
  state: state;
  mutable pixels_from_midtile: int; (* 0-16 *)
  mutable dir: Dir.t;
  segment: Segment.id option; (* for track semaphores *)
  name: string option;
  last_station: Station.id;
  stop_at_station: bool;
  engine: Engine.t;
  cars: Car.t list;
  freight: Goods.freight; (* freight class *)
  _type: train_type;
  history: History.t; (* History of values. Used for cars *)
  stop: int; (* current stop of route *)
  route: stop Utils.Vector.vector; (* route stops *)
  priority: stop option;
  had_maintenance: bool;
  maintenance_cost: int; (* per fin period *)
  dist_traveled: (int ref * int ref); (* by period. Incremented at mid-tiles *)
  dist_shipped_cargo: int * int; (* also by fin period *)
} [@@deriving yojson, show]

let get_route_length v = Vector.length v.route
let get_route_stop v i = Vector.get v.route i

let get_speed v = match v.state with
  | Traveling s -> s.speed
  | WaitingAtStation _ -> 0

let display_speed v = C.speed_mult * get_speed v

let display_maintenance v =
  v.maintenance_cost / 2 + List.length v.cars + C.min_maintenance_cost

let reset_pixels_from_midtile train =
  train.pixels_from_midtile <- 0

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
    state=Traveling {speed=1; target_speed=1; last_stop=(0,0)};
    cars;
    freight=freight_of_cars cars;
    _type=Local;
    history=History.make ();
    stop=0;
    route;
    segment=None;
    name=None;
    last_station=station;
    had_maintenance=false;
    stop_at_station=false;
    priority=None;
    dist_traveled=(ref 0, ref 0);
    dist_shipped_cargo=(0, 0);
    maintenance_cost=0;
  }
  in
  Log.debug (fun f -> f "Train: new train at (%d,%d)" v.x v.y);
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
  let b = (max_speed_factor + 2) * (weight/160 + 1) in
  let engine_speed = (v.engine.Engine.horsepower * 200) / b in
  let random = (13 * idx + cycle) mod 64 in
  Log.debug (fun f -> f "max_sf(%d) weight(%d) random(%d) engine_speed(%d)" max_speed_factor weight random engine_speed);
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
  (* Log.debug (fun f -> f "Train at (%d, %d)" v.x v.y); *)
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

let update_speed (v:t) ~cycle ~cycle_check ~cycle_bit =
  (* Update current train speed based on target speed and cycle *)
  match v.state with
  | Traveling s ->
    if s.target_speed > s.speed then (
      (* accelerate *)
      let speed_diff = min 12 (s.target_speed - s.speed) in
      if s.speed <= 1 ||
         (cycle mod cycle_check = 0 &&
         (update_cycle_array.(speed_diff) land cycle_bit) <> 0) then (
           s.speed <- succ s.speed;
           Log.debug (fun f -> f "Train accelerate. New speed %d" s.speed);
      );
    ) else if s.target_speed < s.speed then (
    (* decelerate *)
      if cycle mod 8 = 0 then (
        s.speed <- s.speed - 1;
        Log.debug (fun f -> f "Train decelerate. New speed %d" s.speed);
      )
    );
    v
  | _ -> v

let update_train idx (train:t) ~cycle ~cycle_check
    ~cycle_bit ~region_div ~update_mid_tile =
  (* let priority = (Goods.freight_to_enum train.freight) * 3 - (Train.train_type_to_enum train._type) + 2 in *)
  match train.state with
  | Traveling travel_state ->
    let train = update_speed train ~cycle ~cycle_check ~cycle_bit in
    (* TODO: fiscal period update stuff *)
    let rec train_update_loop train speed_bound =
      let speed = get_speed train in
      if speed_bound >= speed then train
      else (
        let speed =
          if Dir.is_diagonal train.dir then (speed * 2 + 1) / 3
          else speed
        in
        let update_val =
          if speed > 12 then 
            if speed_bound = 0 then 12 else speed - 12
          else speed
        in
        (* BUGFIX: original code allowed sampling from random memory *)
        let update_val = update_val / region_div
          |> min update_array_length
        in
        (* Log.debug (fun f -> f "Update val %d, cycle_bit %d" update_val cycle_bit); *)
        let train =
          if (update_cycle_array.(update_val) land cycle_bit) <> 0 then (
            (* Log.debug (fun f -> f "Pass test. Update val %d, cycle_bit %d" update_val cycle_bit); *)
            let is_mid_tile =
              (train.x mod C.tile_w) = C.tile_w / 2 &&
              (train.y mod C.tile_h) = C.tile_h / 2
            in
            let loc = train.x / C.tile_w, train.y / C.tile_h in
            if is_mid_tile && Utils.neq_xy travel_state.last_stop loc then
              update_mid_tile train loc
            else
              advance train)
          else
            train
        in
        train_update_loop train (speed_bound + 12)
      )
    in
    train_update_loop train 0
  | WaitingAtStation s when s.wait_time > 0 ->
      s.wait_time <- s.wait_time - 1;
      train
  | WaitingAtStation _ ->
      let loc = train.x / C.tile_w, train.y / C.tile_h in
      update_mid_tile train loc

  (* The algorithm works in segments.
     The length of the full train is car_idx * car_pixels.
     We jump in segments of 16 pixels, which are the length between two 
     mid-tiles. The first car will be the same orientation as the engine,
     if there's sufficient space for it. Otherwise it'll be in the next
     segment, and use that segment's direction history slot.
   *)
let get_car_loc (v:t) car_idx ~car_pixels =
  let total_pixels = car_pixels * (car_idx + 1) in

  let move_back x y dir ~total_pixels ~move_pixels =
    let diag = Dir.is_diagonal dir in
    let x, y = match move_pixels with
      | 16 -> x / 16 * 16 + 8, y / 16 * 16 + 8
      | _ -> x, y
    in
    let move_pixels = if diag then move_pixels * 3 / 2 else move_pixels in
    (* This is critical *)
    let move_pixels = min move_pixels total_pixels in
    let dx, dy = Dir.to_offsets dir in
    let dx, dy = dx * move_pixels, dy * move_pixels in
    let dx, dy = if diag then dx * 2 / 3, dy * 2 / 3 else dx, dy in
    let x, y = x - dx, y - dy in
    let total_pixels = total_pixels - move_pixels in
    x, y, total_pixels
  in
  let rec segment_loop x y i ~total_pixels ~move_pixels =
    let hist = History.get v.history i in
    (* Move to center *)
    let x, y, total_pixels =
      move_back x y hist.dir ~total_pixels ~move_pixels
    in
    if total_pixels <= 0 then 
      x, y, hist.dir
    else
      segment_loop x y (i+1) ~total_pixels ~move_pixels:C.tile_w
  in
  (* TODO: double tracks *)
  segment_loop v.x v.y 0 ~total_pixels ~move_pixels:v.pixels_from_midtile

let get_car_dir (v:t) i =
  (History.get v.history (i+1)).dir
  
