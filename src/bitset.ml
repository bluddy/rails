open Containers

module type Elem = sig
  type t
  val of_enum : int -> t option
  val to_enum: t -> int
  val last: t
end

module type S = sig

  type elt

  type t

  val empty: t

  val is_empty: t -> bool

  val mem: t -> elt -> bool

  val remove: t -> elt -> t

  val add: t -> elt -> t

  val singleton: elt -> t

  val equal: t -> t -> bool

  val fold: ('a -> elt -> 'a) -> 'a -> t -> 'a

  val iter: (elt -> unit) -> t -> unit

  val of_list: elt list -> t

  val to_list: t -> elt list

  val to_int: t -> int

  val pp: Format.formatter -> t -> unit

end

module Make(E: Elem) = struct

  type elt = E.t
  type t = int

  let to_int v = v

  let empty = 0

  let is_empty v =
    v = 0

  let mem v elem =
    let i = E.to_enum elem in
    v land (1 lsl i) <> 0

  let remove v elem =
    let i = E.to_enum elem in
    v land (lnot (1 lsl i))

  let add v elem =
    let i = E.to_enum elem in
    v lor (1 lsl i)

  let singleton elem =
    add empty elem

  let equal v1 v2 =
    v1 = v2

  let fold f zero v =
    let acc = ref zero in
    for i=0 to (E.to_enum E.last) do
      if ((1 lsl i) land v) <> 0 then
        acc := f !acc (E.of_enum i |> Option.get_exn_or "of_enum");
    done;
    !acc

  let iter f v =
    for i=0 to (E.to_enum E.last) do
      if ((1 lsl i) land v) <> 0 then
        f (E.of_enum i |> Option.get_exn_or "iter");
    done

  let of_list l =
    let v = ref empty in
    List.iter (fun elem ->
      v := add !v elem
    )
    l;
    !v

  let to_list v =
    fold (fun acc x -> x::acc) [] v

  let pp fmt v =
    Format.fprintf fmt "%d" v

end
