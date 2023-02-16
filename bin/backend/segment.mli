type id
[@@deriving yojson]

module Map : sig
  type t [@@deriving yojson]
  val make : unit -> t
  val get_id : t -> id
  val incr_train: t -> id -> unit
  val decr_train: t -> id -> unit
end
