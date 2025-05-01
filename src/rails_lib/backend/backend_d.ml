open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers
open! Utils
open Utils.Infix
(* Backend data *)

type t = {
  params: Params.t;
  mutable last_tick: int; (* last time we updated a cycle *)
  pause: bool;  (* pause the backend. Do not advance time *)
  players: Player.t array; (* stats, money *)
  map : Tilemap.t;
  mutable track: Trackmap.t;
  mutable graph: Track_graph.t;
  cities: Cities.t;
  engines: Engine.t list;
  mutable stations: Station_map.t;
  mutable blocks: Block_map.t; (* map blocks btw stations *)
  mutable dev_state: Tile_develop.t;
  stocks: Stock_market.t;
  ai: Ai.t;
  mutable ui_msgs: Ui_msg.t list;
  random: Utils.Random.State.t;
  seed: int;
} [@@deriving yojson]

let update_player v player f =
  let p = v.players.(player) in
  let p' = f p in
  if p =!= p' then
    v.players.(player) <- p';
  ()


