open Containers

module type Elem = sig
  type t [@@deriving yojson]
  val of_enum : int -> t option
  val to_enum: t -> int
  val last: t
end

module type S = sig

  type elt [@@deriving yojson]

  type t [@@deriving yojson]

  val empty: t

  val is_empty: t -> bool

  val mem: t -> elt -> bool

  val remove: elt -> t -> t

  val find: t -> (elt -> bool) -> elt

  val find_opt: t -> (elt -> bool) -> elt option

  val add: elt -> t -> t

  val singleton: elt -> t

  val equal: t -> t -> bool

  val fold: ('a -> elt -> 'a) -> 'a -> t -> 'a

  val iter: (elt -> unit) -> t -> unit

  val cardinal: t -> int

  (** Remove element from set. Raise Not_found on empty set *)
  val pop: t -> (elt * t)

  val pop_opt: t -> (elt * t) option

  val toggle: elt -> t -> t

  val of_list: elt list -> t

  val to_list: t -> elt list

  val to_int: t -> int

  val hash: t -> int

  val pp: Format.formatter -> t -> unit

end

module Make (E : Elem) : S with type elt = E.t
