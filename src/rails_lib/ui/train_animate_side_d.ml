
type t = {
  mutable last_time: int; (* last time updated *)
  mutable x: int;
  mutable ctr: int;
  rail: [`Back | `Front];
  engine: Engine.make;
  cars: Goods.t list;
  station: int * int;
  paused: bool;
}
