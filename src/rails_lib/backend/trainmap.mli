
module Id : sig include module type of Int_id.Make(struct end) end

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

val iteri: (int -> ro Train.t -> unit) -> t -> unit

val fold: ('a -> ro Train.t -> 'a) -> t -> init:'a -> 'a

val foldi: (int -> 'a -> ro Train.t -> 'a) -> t -> init:'a -> 'a

val mapi_in_place: (int -> rw Train.t -> rw Train.t) -> t -> t

val fold_mapi_in_place: (int -> 'a -> rw Train.t -> 'a * rw Train.t) -> t -> init:'a -> 'a

val find_ret_index: (ro Train.t -> bool) -> t -> Id.t option

val get_at_loc: Utils.loc -> t -> Id.t list

val total_engine_value: t -> Money.t

val total_car_value: t -> Money.t

val total_maintenance: t -> Money.t

val clear_priority_shipment: t -> t

