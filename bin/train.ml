open Containers

type t = {
  x: int;
  y: int;
  engine: Engine.t;
  dir: Dir.t;
  speed: int;
  target_speed: int;
  cars: (Goods.t * int) list; (* good type, level to 160 *)
  freight: Goods.freight;
} [@@deriving yojson]

let freight_of_cars cars =
  List.fold_left (fun freight car ->
    let freight2 = Goods.freight_of_goods car in
    if Goods.compare_freight freight2 freight > 0
    then freight2 else freight)
  Goods.FreightMail
  cars

let make x y engine cars =
  {
    x; y;
    engine;
    dir=Dir.Up;
    speed=0;
    target_speed=0;
    cars=List.map (fun x -> (x,0)) cars;
    freight=freight_of_cars cars;
  }

