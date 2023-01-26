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

  let make () =
    {
      history=Array.make Constants.train_max_size empty;
      idx=0;
    }

  (* Saves the info about the train over time at midpoints *)
  let add v x y dir speed_factor =
    let hist = {x; y; dir; speed_factor} in
    v.idx <- (succ v.idx) mod Array.length v.history;
    v.history.(v.idx) <- hist;
    ()
end

type t = {
  mutable x: int;
  mutable y: int;
  engine: Engine.t;
  mutable dir: Dir.t;
  mutable speed: int;
  mutable wait_time: int; (* for updating train *)
  target_speed: int;
  cars: (Goods.t * int) list; (* good type, amount to 160, /4 = tons *)
  freight: Goods.freight; (* freight class *)
  _type: train_type;
  history: History.t; (* History of values. Used for cars *)

  target_stop: int; (* current stop of route *)
  route: stop list; (* route stops *)
  priority: stop option;
} [@@deriving yojson]

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

let make (x, y) engine cars other_station ~dir =
  let route = [make_stop x y None] in
  let route = match other_station with
    | Some (x,y) -> route @ [make_stop x y None]
    | None -> route
  in
  {
    x=x * Constants.tile_w;
    y=y * Constants.tile_h;
    engine;
    dir;
    speed=0;
    target_speed=0;
    cars=List.map (fun x -> (x,0)) cars;
    freight=freight_of_cars cars;
    wait_time=0;
    _type=Local;
    history=History.make ();
    target_stop=0;
    route;
    priority=None;
  }

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
let update_cycle_array = [| 0; 1; 0x41; 0x111; 0x249; 0x8A5; 0x555; 0x5AD; 0x6DB; 0x777; 0x7DF; 0x7FF; 0xFFF |]

let update_speed (v:t) ~cycle ~cycle_check ~cycle_bit =
  (* Check accelerate *)
  let speed_diff = max 12 @@ v.target_speed - v.speed in
  if (cycle mod cycle_check) = 0 && v.speed > 1 &&
     (update_cycle_array.(speed_diff) land cycle_bit) <> 0 then begin
      v.speed <- succ v.speed
  end;
  (* Check decelerate *)
  if cycle mod 8 = 0 && v.target_speed < v.speed then begin
    v.speed <- pred v.speed
  end

