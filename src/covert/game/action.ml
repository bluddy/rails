open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

type kind =
  | Default
  | Travel
  | Break_in
  | Item_confiscate
  | Item_spotted
  | Agent_turn
  | Agent_hide
  | Agent_arrest
  | Agent_move
  | Agent_escape
  | Agent_exchange
  | Agent_out_of_hiding
  [@@deriving yojson]

type known = [
  | `Decoded
  | `Known_time
  | `Known_name
  | `Known_org
  | `Known_loc
] [@@deriving yojson, ord]

module KnownSet = Utils.Set.Make(struct
  type t = known [@@deriving yojson, ord]
end)

type t = {
  kind: kind;
  time: int;
  (* low bit 0/1 send/rcv *)
  event: Event.Id.t option;
  known: KnownSet.t;
  (* agent status/anxiety in OG *)
  agent_send: Agent.Id.t;
  loc_send: Loc.Id.t;
  agent_rcv: Agent.Id.t;
  loc_rcv: Loc.Id.t;
} [@@deriving yojson]

