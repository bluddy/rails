open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

type status =
  | Ready
  | Tick of int
  [@@deriving yojson,show]

type info =
  | EmptyBits (* not sure what this signifies *)

module Kind = struct
  type kind_ =
    | Sent_msg
    | Meeting
    | Rcv_msg
    | Use_anxiety
    | Misc_action
    | Event_known
    | Send_package
    [@@deriving yojson, ord]

  module Set = Utils.Set.Make(struct
    type t = kind_ [@@deriving yojson, ord]
  end)

  type t = kind_ [@@deriving yojson]
end


type t = {
  role: Role.Id.t;
  rcv_role: Role.Id.t option;
  status: status;
  num_id: int;
  text: string;
  bits: int;
  item_bits: int;
  efficiency: int;
  kind: Kind.Set.t;
} [@@deriving yojson]

module Id = Event_id

module Map = Event_id.Map

type map = t Map.t [@@deriving yojson]

