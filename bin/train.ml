open Sexplib.Std
open Containers

type t = {
  x: int;
  y: int;
  engine: Engine.t;
  dir: Dir.t;
  speed: int;
  target_speed: int;
  cars: Goods.t list;
} [@@deriving sexp]

let make x y engine cars =
  {
    x; y;
    engine;
    dir=Dir.Up;
    speed=0;
    target_speed=0;
    cars;
  }

