open Containers
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
  consist_change: (Goods.t list) option; (* consist. None -> "No Change" *)
} [@@deriving yojson, show]

let make_stop x y consist_change = {x; y; consist_change}

let make_nowait_stop x y consist_change = (`NoWait, make_stop x y consist_change)

type train_type =
  | Local (* Stops at every stop *)
  | Through (* Skips depots *)
  | Express (* Skips stations or less *)
  | Limited (* Skips all *)
  [@@deriving yojson, enum, eq, show {with_path = false}]

module History = struct
  (* History is used to draw the cars behind the engine *)
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
  let get_freight v = Freight.of_good @@ get_good v
  let get_age v cycle = match v.load with
    | Some load -> cycle - load.cycle
    | None -> assert false
  let get_source v = match v.load with
    | Some {source;_} -> source
    | None -> assert false
  let get_amount v = match v.load with
    | Some load -> load.amount
    | None -> 0
  let is_full v = get_amount v = C.car_amount

  let empty v = {v with load=None}

  let get_load v = match v.load with
    | None -> None
    | Some load ->
      Some (v.good, load.amount, load.source)

end

type state =
  | Traveling of { mutable speed: int; (* x5 to get real speed *)
                   mutable target_speed: int;
                   (* To prevent double processing, we turn on this flag while at a station location *)
                   mutable traveling_past_station: bool;
                   block: Block_map_d.Id.t; 
                }
  | LoadingAtStation of {mutable wait_time: int} (* Normal loading time *)
  | WaitingForFullLoad (* In a station with Wait *)
  | HoldingAtStation (* Held and waiting for unhold by player *)
  | StoppedAtSignal of Dir.t (* Waiting at a hold signal *)
  [@@deriving yojson, show]

type periodic = {
  mutable dist_traveled: int;
  mutable ton_miles: int;
  revenue: int;
} [@@deriving yojson, show]

type rw = Utils.Infix.rw
type ro = Utils.Infix.ro

type wait = [`Wait | `NoWait]
  [@@ deriving yojson, show]

let is_wait = function `Wait -> true | `NoWait -> false

(* 'mut is so we can't mutate a train from the wrong api *)
type 'mut t = {
  mutable x: int; (* in map pixels *)
  mutable y: int; (* in map pixels *)
  player: int;
  state: state;
  (* Used for train/car drawing algorithm *)
  mutable pixels_from_midtile: int; (* 0-16 *)
  mutable dir: Dir.t;
  (* Only record trains can be named, and it increases passenger income by 25% *)
  name: string option;
  last_station: Station.id; (* Could be far away due to express *)
  hold_at_next_station: bool;
  engine: Engine.t;
  cars: Car.t list;
  freight: Freight.t; (* freight class. Based on majority of cars *)
  typ: train_type; (* How many stations we stop at *)
  history: History.t; (* History of values. Used for cars *)
  stop: int; (* current stop of route *)
  route: (wait * stop) Utils.Vector.vector; (* route stops *)
  priority: stop option; (* priority stop *)
  had_maintenance: bool; (* this period *)
  maintenance_cost: int; (* per fin period *)
  periodic: periodic * periodic; (* data saved for periodic tracking *)
} [@@deriving yojson, show]

let get_route_length v = Vector.length v.route
let get_route_stop v i = Vector.get v.route i

let get_speed v = match v.state with
  | Traveling s -> s.speed
  | _ -> 0

let is_traveling v = match v.state with
  | Traveling _ -> true
  | _ -> false

let set_type v typ = {v with typ}
let replace_engine v engine = {v with engine; maintenance_cost=0}

let display_speed v = C.speed_mult * get_speed v

let display_maintenance v =
  v.maintenance_cost / 2 + List.length v.cars + C.min_maintenance_cost

let reset_pixels_from_midtile (train: rw t) =
  train.pixels_from_midtile <- 0

let get_route_dest v = Vector.get v.route v.stop

let get_next_stop v =
  match v.priority with
  | Some stop -> (`NoWait, stop)
  | None -> get_route_dest v

let get_dest v = 
  let stop = get_next_stop v |> snd in
  (stop.x, stop.y)

let freight_of_cars cars =
  (* Freight goes by the highest level *)
  List.fold_left (fun freight car ->
    let freight2 = Car.get_freight car in
    if Freight.compare freight2 freight > 0
    then freight2 else freight)
  `Mail
  cars

let freight_set_of_cars cars =
  (* Create a complex freight set *)
  List.fold_left (fun set car ->
    let freight = Car.get_freight car in
    Freight.Set.add set freight)
  Freight.Set.empty
  cars


let get_car_goods_count cars =
  let h = Hashtbl.create 10 in
  List.iter (fun car -> Hashtbl.incr ~by:1 h car.Car.good) cars;
  h

let make ((x,y) as station) engine cars other_station ~dir ~player =
  let route = [`NoWait, make_stop x y None] in
  let route = match other_station with
    | Some (x,y) -> [`NoWait, make_stop x y None] @ route
    | None -> route
  in
  let route = Vector.of_list route in
  let cars = List.map Car.make cars in
  let make_periodic () = {
      dist_traveled=0;
      ton_miles=0;
      revenue=0;
    }
  in
  let v = {
    x=x * C.tile_w + C.tile_w / 2;
    y=y * C.tile_h + C.tile_h / 2;
    engine;
    pixels_from_midtile=0;
    dir;
    state=LoadingAtStation{wait_time=0};
    cars;
    freight=freight_of_cars cars;
    typ=Local;
    history=History.make ();
    stop=0;
    route;
    name=None;
    last_station=station;
    had_maintenance=false;
    hold_at_next_station=false;
    priority=None;
    maintenance_cost=0;
    periodic=(make_periodic (), make_periodic ());
    player;
  }
  in
  Log.debug (fun f -> f "Train: new train at (%d,%d)" v.x v.y);
  v

let get_route v = v.route

let get_stop v i = Vector.get v.route i

let remove_stop_car (v:rw t) stop car =
  let remove_car (stop:stop) =
    let consist_change = match stop.consist_change with
      | None -> None
      | Some car_list ->
          begin match List.remove_at_idx car car_list with
          | [] -> None
          | l  -> Some l
          end
    in
    {stop with consist_change}
  in
  match stop with
  | `Stop stop_idx ->
      Vector.modify_at_idx v.route stop_idx (fun x -> Utils.map_snd remove_car x);
      v
  | `Priority ->
      let priority = Option.map remove_car v.priority in
      {v with priority}

let add_stop_car (v:rw t) stop car =
  let add_car (stop:stop) =
    let consist_change = match stop.consist_change with
      | Some car_list -> Some(car_list @ [car])
      | None -> Some([car])
    in
    {stop with consist_change}
  in
  match stop with
  | `Stop stop_idx ->
      Vector.modify_at_idx v.route stop_idx (fun x -> Utils.map_snd add_car x);
      v
  | `Priority ->
      let priority = Option.map add_car v.priority in
      {v with priority}

let remove_all_stop_cars (v:rw t) stop =
  let remove_all_cars (stop:stop) = {stop with consist_change = Some []} in
  match stop with
  | `Stop stop_idx ->
      Vector.modify_at_idx v.route stop_idx (fun x -> Utils.map_snd remove_all_cars x);
      v
  | `Priority ->
      let priority = Option.map remove_all_cars v.priority in
      {v with priority}

let check_stop_station (v:'a t) stop loc =
  (* Don't allow setting the station if the previous or next station
     is the same station already *)
  let len = Vector.length v.route in
  let prev, next = match stop with
    | 0 when len >= 2 -> None, Some 1
    | 0 -> None, None
    | s when len > s + 1 -> Some (s-1), Some (s+1)
    | s -> Some (s-1), None
  in
  let check = function
    | Some i ->
        let stop = Vector.get v.route i |> snd in
        Utils.eq_xy loc (stop.x, stop.y)
    | None -> false
  in
  not (check prev || check next)

let set_stop_station (v:rw t) stop (x,y) =
  match stop with
  | `Stop i ->
      (* Check for lengthening *)
      if (Vector.length v.route) = i then begin
        Vector.push v.route (make_nowait_stop x y None)
      end else begin
        Vector.modify_at_idx v.route i (fun (w, (stop:stop)) -> (w, {stop with x; y}))
      end;
      v
  | `Priority ->
      let stop = match v.priority with
        | None -> make_stop x y None
        | Some stop -> {stop with x; y}
      in
      {v with priority=Some stop}

let remove_stop (v:rw t) stop =
  match stop with
  | `Stop i ->
      Vector.remove_and_shift v.route i;
      v
  | `Priority -> {v with priority=None}

let toggle_stop_wait (v:rw t) stop =
  Vector.modify_at_idx v.route stop (fun (wait, stop) ->
    let wait2 = match wait with `Wait -> `NoWait | `NoWait -> `Wait in
    (wait2, stop)
  );
  v

(* Cycles used to update integer speed up to index=12
   Note that each step has one more active bit
 *)
let update_cycle_array =
  [| 0; 1; 0x41; 0x111; 0x249; 0x8A5; 0x555; 0x5AD; 0x6DB; 0x777; 0x7DF; 0x7FF; 0xFFF |]

let update_array_length = Array.length update_cycle_array

let get_weight v =
  List.fold_left (fun weight car ->
    let freight = Freight.of_good car.Car.good |> Freight.to_enum in
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

let get_period v period = match period, v.periodic with
  | `First, (p, _) -> p
  | `Second, (_, p) -> p

let get_revenue v period = 
  let period = get_period v period in
  period.revenue

let update_period v period f = match period, v.periodic with
  | `First,  (p, x) -> {v with periodic=(f p, x)}
  | `Second, (x, p) -> {v with periodic=(x, f p)}

let add_dist_traveled (v: rw t) add period =
  let period = get_period v period in
  period.dist_traveled <- period.dist_traveled + add

let add_ton_miles (v: rw t) add period =
  let period = get_period v period in
  period.ton_miles <- period.ton_miles + add

let is_full (v: 'a t) = List.for_all Car.is_full v.cars

let advance (v:rw t) =
  (* Always advance train by single pixel *)
  let dx, dy = Dir.to_offsets v.dir in
  v.x <- v.x + dx;
  v.y <- v.y + dy;
  v.pixels_from_midtile <- succ v.pixels_from_midtile;
  (* Log.debug (fun f -> f "Train at (%d, %d)" v.x v.y); *)
  v

let check_increment_stop v (x,y) =
  (* Increment the next stop on the route *)
  match v.priority, Vector.get v.route v.stop |> snd with
  | Some stop, _ when stop.x = x && stop.y = y ->
      (* When exiting priority mode, we go to the closest station *)
      let min_stop_idx =
        Vector.foldi (fun i ((min_dist,_) as acc) (_, (stop: stop)) ->
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
  let freight = Freight.to_enum @@ Car.get_freight car in
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

let update_speed (v:rw t) ~cycle ~cycle_check ~cycle_bit =
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


  let adjust_loc_for_double_track trackmap x y dir =
    (* Handle double tracks *)
    match Trackmap.get trackmap ~x:(x/C.tile_w) ~y:(y/C.tile_h) with 
    | Some track when Track.acts_like_double track ->
        let mult = match dir with
          | Dir.DownLeft | UpLeft -> 1
          | _ -> 2
        in
        let adjust_dir = dir |> Dir.cw |> Dir.cw in
        let dx, dy = Dir.to_offsets adjust_dir in
        x + dx * mult, y + dy * mult
    | _ -> x, y


  (* The algorithm works in segments.
     The length of the full train is car_idx * car_pixels.
     This function 'queries' the location of each car by pixel numbers (car_idx * car_pixels).
     The original function only queries by pixel, which can be faked by giving any
     number for the car_idx and car_pixels.
     We jump in segments of 16 pixels, which are the length between two 
     mid-tiles. The first car will be the same orientation as the engine,
     if there's sufficient space for it. Otherwise it'll be in the next
     segment, and use that segment's direction history slot, etc.
   *)
let calc_car_loc_in_pixels (v:'a t) trackmap total_pixels =
  let move_back x y dir ~total_pixels ~move_pixels =
    let diag = Dir.is_diagonal dir in
    let x, y =
      if move_pixels = C.tile_w then
        x / C.tile_w * C.tile_w + C.tile_hdim, y / C.tile_h * C.tile_h + C.tile_hdim
      else
        x, y
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
    let x, y, total_pixels = move_back x y hist.dir ~total_pixels ~move_pixels in
    if total_pixels <= 0 then 
      x, y, hist.dir
    else
      segment_loop x y (i+1) ~total_pixels ~move_pixels:C.tile_w
  in
  let x, y, dir =
    segment_loop v.x v.y 0 ~total_pixels ~move_pixels:v.pixels_from_midtile
  in
  let x, y = adjust_loc_for_double_track trackmap x y dir in
  x, y, dir

let calc_car_loc (v:'a t) trackmap car_idx ~car_pixels =
  (* Same function, with a per-car interface *)
  let total_pixels = car_pixels * (car_idx + 1) in
  calc_car_loc_in_pixels v trackmap total_pixels


let get_car_dir (v:'a t) i = (History.get v.history (i+1)).dir

let get_engine_cost v = v.engine.price

let get_num_of_cars v = List.length v.cars


