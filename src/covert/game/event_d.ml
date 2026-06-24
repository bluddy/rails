open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module String = Engine.String
module Gen = Engine.My_gen

type status =
  | Ready (* 999 in OG *)
  | Unused (* -1 in OG *)
  | Tick of int
  [@@deriving yojson,show]

type info =
  | EmptyBits (* not sure what this signifies *)

type interaction = Meet | Msg [@@deriving yojson]
type tx = Send | Rcv [@@deriving yojson]
type kind =
  | With_role of {
    inter: interaction;
    tx: tx;
    rcv_role: Role.Id.t;
  }
  | Misc
  | Terminal
  [@@deriving yojson]

type items = {
   rcv: Item.Id.t list;
   send: Item.Id.t list;
  } [@@deriving yojson]

type t = {
  role: Role.Id.t;
  status: status;
  num_id: int;
  text: string;
  bits: int;
  items: items;
  efficiency: int;
  kind: kind;
  incapacitated_ok: bool; (* OK to have agent out of action *)
} [@@deriving yojson]

module Id = Event_id

module Map = Event_id.Map

type map = t Map.t [@@deriving yojson]

let has_role event = match event.kind with
  | With_role _ -> true
  | _ -> false

let is_ready event = match event.status with Ready -> true | _ -> false
let status v = v.status
let update_status status v = {v with status}
let set_tick tick v = {v with status=Tick tick}
let is_misc event = match event.kind with Misc -> true | _ -> false

let from_stream ~num_events s =
  Iter.fold (fun acc _ ->
    let role = Gen.get_bytei s |> Role.Id.of_int in
    let _junk = Gen.get_bytei s in
    let _tick_or_status = Gen.get_wordi s in
    let num_id = Gen.get_wordi s in
    let text = Gen.take 32 s |> Gen.to_stringi |> String.remove_nulls in
    let bits = Gen.get_wordi s in
    let item_bits = Gen.get_wordi s in
    let rcv_items = item_bits land 0xFF
    |> Utils.bits_to_int_list |> List.map Item.Id.of_int in
    let send_items = (item_bits land 0xFF00) lsr 8
    |> Utils.bits_to_int_list |> List.map Item.Id.of_int in
    let efficiency = Gen.get_wordi s in
    let incapacitated_ok = bits land 0x1000 > 0 in
    let kind =
      if bits land 0x2000 > 0 then Misc else
      if Role.Id.(role = of_int 255) then Terminal else
      let rcv_role = bits land 0xFF |> Role.Id.of_int in
      let inter = if bits land 0x800 > 0 then Meet else begin
        assert (bits land 0x200 > 0);
        Msg
        end
      in
      let tx = if bits land 0x100 > 0 then Rcv else Send in
      With_role {inter; tx; rcv_role}
    in
    let event = {
      role;
      status=Ready;
      num_id;
      text;
      bits;
      items={rcv=rcv_items; send=send_items};
      efficiency;
      kind;
      incapacitated_ok;
    } in
    print_endline @@ (yojson_of_t event |> Yojson.Safe.to_string);
    event::acc
  )
  []
  Iter.(0 -- (num_events - 1)) |> List.rev

