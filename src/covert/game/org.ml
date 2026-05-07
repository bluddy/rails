
type t = {
  short_name: string;
  name: string;
  connect: int * int;
  strength: int;
  hq_build_cost: int;
  bits: int;
  agent_name_offset: int; (* offset into the name list *)
} [@@deriving show]

module Id = Engine.Int_id.Make()

