type id
[@@deriving yojson, eq]

module Map : sig
  type t [@@deriving yojson]
  val make : unit -> t
  val get_id : t -> id
  val incr_train: t -> id -> unit
  val decr_train: t -> id -> unit
  val merge: t -> id -> id -> unit
end
