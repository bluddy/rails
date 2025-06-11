
type delivery = {
  src: Utils.loc;
  amount: int;
  revenue: Money.t;
}

type t = {
  anim: Train_animate_side_d.t;
  finished: bool;
  good: Goods.t;
  loc: Utils.loc;
  delivery: delivery option;
}
