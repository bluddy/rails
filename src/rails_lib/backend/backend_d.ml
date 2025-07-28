open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Utils
(* Backend data *)

type t = {
  params: Params.t;
  mutable last_tick: int; (* last time we updated a cycle *)
  pause: bool;  (* pause the backend. Do not advance time *)
  mutable players: Player.t Owner.Map.t; (* stats, money *)
  map : Tilemap.t;
  mutable track: Trackmap.t;
  mutable graph: Track_graph.t;
  cities: Cities.t;
  engines: Engine.t list;
  mutable stations: Station_map.t;
  mutable blocks: Block_map.t; (* map blocks btw stations *)
  mutable dev_state: Tile_develop.t; (* State for gradual map developmnt *)
  stocks: Stock_market.t;
  ai: Ai.t;
  delayed_fn: (Owner.t -> t -> t) option; (* We wait for the UI to advance our delayed action *)
  mutable ui_msgs: Ui_msg.t list;
  random: Utils.Random.State.t;
  seed: int;
} [@@deriving yojson]


let update_player v idx f =
  let players = Player.update v.players idx f in
  [%up {v with players}]

let update_player_state v idx f =
  (* Update player with mutable state, for fast changes *)
  let p = Owner.Map.find idx v.players in
  let () = f p in
  ()

