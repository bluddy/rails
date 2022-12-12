open Containers

(* A leg of a route *)
type leg = {
  x: int;
  y: int;
  cars: Goods.t list;
} [@@deriving yojson]

type t = {
  x: int;
  y: int;
  engine: Engine.t;
  dir: Dir.t;
  speed: int;
  target_speed: int;
  cars: (Goods.t * int) list; (* good type, amount to 160, /4 = tons *)
  freight: Goods.freight; (* freight class *)

  leg: int; (* current leg of route *)
  route: leg list; (* route stops *)
  priority: leg option;
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
    leg=0;
    route=[];
    priority=None;
  }

