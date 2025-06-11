type loc = Utils.loc

type delivery = {
  src: Utils.loc;
  amount: int;
  revenue: Money.t;
  speed: int;
}

type pickup = {
  buying: loc list; (* places that can buy the good *)
}

type t = {
  anim: Train_animate_side_d.t;
  finished: bool;
  good: Goods.t;
  loc: Utils.loc;
  kind: [`Delivery of delivery | `Pickup of pickup]
}
