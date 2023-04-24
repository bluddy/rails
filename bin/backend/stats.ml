open Containers

type t = {
  mutable dist_traveled: int;
  goods_shipped: (int * int):
  freight_shipped: (Goods.freight, int) Hashtbl.t;
  goods_delivered: Goods.Set.t;
  freight_income: int array;
  other_income: int;
} [@@ deriving yojson]

let default = {
  dist_traveled=0;
  goods_shipped=(0, 0);
  freight_shipped=Hashtbl.make ();
  goods_delivered=Goods.Set.empty;
  freight_income=Array.make (Goods.num_freight) 0;
  other_income=0;
}
