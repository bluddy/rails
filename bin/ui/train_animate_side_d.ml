
type t = {
  mutable last_time: int; (* last time updated *)
  mutable x: int;
  mutable anim_idx: int; (* animation index for wheels *)
  mutable smoke_idx: int; (* animation index for smoke *)
  rail: [`Back | `Front];
  engine: Engine.make;
  cars: Goods.t list;
  station: int * int;
  paused: bool;
}
