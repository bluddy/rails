
type t = {
  mutable last_time: int; (* last time updated *)
  mutable x: int;
  y: int;
  mutable anim_idx: int; (* animation index for wheels *)
  mutable smoke_idx: int; (* animation index for smoke *)
  rail: [`Back | `Front];
  engine: Engine.make;
  cars: Goods.t list;
  visible: bool;
  pause_at_cars: bool option;
  station_x: int;
  station_y: int;
  moving: bool;
}
