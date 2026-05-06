
type t = {
  short_name: string;
  name: string;
  connect: int * int;
  strength: int;
  hq_build_cost: int;
  bits: int;
  related_agent: int;
} [@@deriving show]

module Id = Engine.Int_id.Make()

