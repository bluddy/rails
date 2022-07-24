
type t = {
  mutable x: int;
  y: int;
  mutable anim_idx: int; (* animation index for wheels *)
  mutable smoke_idx: int; (* animation index for smoke *)
  mutable last_time: int; (* last time updated *)
  rail: [`Back | `Front];
  engine: Engine.make;
  cars: Goods.t list;
  visible: bool;
  pause_at_cars: bool option;
  station_x: int;
  station_y: int;
}

let default ?(pause_at_cars=false) ~engine ~cars ~station_x ~station_y =
  {
    x=0;
    y=0;
    anim_idx=0;
    smoke_idx=0;
    last_time=0;
    rail=`Back;
    engine;
    cars;
    visible=false;
    pause_at_cars=if pause_at_cars then Some false else None;
    station_x;
    station_y;
  }
