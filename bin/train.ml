open Containers

type t = {
  x: int;
  y: int;
  engine: Engine.make;
  dir: Dir.t;
  speed: int;
  target_speed: int;
  cars: Goods.t list;
}

let make x y engine cars =
  {
    x; y;
    engine;
    dir=Dir.Up;
    speed=0;
    target_speed=0;
    cars;
  }

module Map = struct
  type nonrec t = {
    map: (t, CCVector.rw) CCVector.t;
  }
  [@@deriving sexp]

  let add v train =
    CCVector.push v.map train; v

  let delete v idx =
    CCVector.remove_and_shift v.map idx; v
end

