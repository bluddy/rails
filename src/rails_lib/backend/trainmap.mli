
module Id = Train.Id

type ro = Train.ro
type rw = Train.rw

type t [@@deriving yojson]

val empty : unit -> t

val get: Id.t -> t -> ro Train.t

val add : rw Train.t -> t -> t

val delete : Id.t -> t -> t

val update: Id.t -> t -> (rw Train.t -> rw Train.t) -> t

val size: t -> int

val get_last: t -> ro Train.t

val iter: (ro Train.t -> unit) -> t -> unit

val iteri: (Id.t -> ro Train.t -> unit) -> t -> unit

val fold: ('a -> ro Train.t -> 'a) -> t -> init:'a -> 'a

val foldi: (Id.t -> 'a -> ro Train.t -> 'a) -> t -> init:'a -> 'a

val mapi_in_place: (Id.t -> rw Train.t -> rw Train.t) -> t -> t

val fold_mapi_in_place: (Id.t -> 'a -> rw Train.t -> 'a * rw Train.t) -> t -> init:'a -> 'a

val fold_mapi_by_priority: (Id.t -> 'a -> rw Train.t -> 'a * rw Train.t) -> t -> init:'a -> 'a * t

val find_ret_index: (ro Train.t -> bool) -> t -> Id.t option

val get_at_loc: Utils.loc -> t -> Id.t list

val total_engine_value: t -> Money.t

val total_car_value: t -> Money.t

val total_maintenance: t -> Money.t

val clear_priority_shipment: t -> t

val remove_goods_in_all_trains: Goods.Set.t -> t -> t

val subrange: Id.t -> num: int -> t -> int Iter.t * [`First | `NotFirst] * [`Last | `NotLast]

val first_train: t -> Id.t

val find_trains_in_range: x:int -> y:int -> range:int -> t -> Id.t list

