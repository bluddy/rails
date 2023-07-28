open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers
open Utils
open Utils.Infix
(* Backend data *)

let tick_ms = 15 (* ms *)
let year_ticks = 2032 (* really 170*12 = 2040 is new year *)
let month_ticks = 170
let num_players = 4

(* Cycle counts to perform some tasks *)
let cycles_periodic_maintenance = 1024
let cycles_priority_delivery = 8
let cycles_background_update = 16
let cycles_ai_update = cycles_background_update
(* In the original game, we do slices of 1/32 stations. No need *)
let cycles_station_supply_demand = cycles_background_update * 32 (* 512 *)
(* Since we don't spread out the supply addition, decay happens in the same cycle *)
let cycles_supply_decay = 512

type ui_msg =
  | TrainBuilt of int
  | DemandChanged of {x: int; y: int; good: Goods.t; add: bool}
  [@@deriving yojson]

type t = {
  mutable last_tick: int; (* last time we updated a cycle *)
  mutable cycle: int; (* counter used for all sorts of per-tick updates *)
  mutable time: int;  (* In-game time, resets at end of year *)
  mutable year: int;
  pause: bool;  (* pause the backend. Do not advance time *)
  year_start: int;
  fiscal_period: [`First | `Second];
  climate: Climate.t;
  west_us_route_done: bool;
  players: Player.t array; (* stats, money *)
  region: Region.t;
  map : Tilemap.t;
  mutable track: Trackmap.t;
  mutable graph: Track_graph.t;
  trains: Trainmap.t;
  cities: Cities.t;
  engines: Engine.t list;
  mutable stations: Station_map.t;
  segments: Segment.Map.t; (* map segments btw stations *)
  priority: (loc * loc * Goods.t) option;  (* priority shipment *)
  options: B_options.t;
  mutable ui_msgs: ui_msg list;
  random: Utils.Random.State.t;
  seed: int;
} [@@deriving yojson]

let update_player v player f =
  let p = v.players.(player) in
  let p' = f p in
  if p =!= p' then
    v.players.(player) <- p';
  ()


