open Containers
module Hashtbl = Utils.Hashtbl

type t = {
  mutable dist_traveled: int;
  goods_shipped_dist: (int * int);
  freight_shipped: (Goods.freight, int) Hashtbl.t;
  goods_delivered: Goods.Set.t;
  freight_income: (Goods.freight, int) Hashtbl.t;
  other_income: int;
} [@@ deriving yojson]

let default = {
  dist_traveled=0;
  goods_shipped_dist=(0, 0);
  freight_shipped=Hashtbl.create 10;
  goods_delivered=Goods.Set.empty;
  freight_income=Hashtbl.create 10;
  other_income=0;
}
