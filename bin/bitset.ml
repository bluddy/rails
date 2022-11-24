open Containers

module type Elem = sig
  type t
  val of_enum : int -> t option
  val to_enum: t -> int
  val last: t
end

module type S = [%import: (module Bitset.S)]

module Make(E: Elem) = struct

  type elt = E.t

  type t = int [@@ deriving yojson]

  let to_int v = v

  let of_int (v:int) = v

  let empty = 0

  let is_empty v =
    v = 0

  let mem v elem =
    let i = E.to_enum elem in
    v land (1 lsl i) <> 0

  let remove v elem =
    let i = E.to_enum elem in
    v land (lnot (1 lsl i))

  let find v f =
    let exception Found of elt in
    try
      for i=0 to (E.to_enum E.last) do
        let value = E.of_enum i |> Option.get_exn_or "error" in
        if ((1 lsl i) land v) <> 0 && f value then
            raise_notrace (Found(value))
      done;
      raise Not_found
    with
    Found x -> x

  let find_opt v f =
    try
      find v f |> Option.some
    with
    Not_found -> None

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

  let cardinal v =
    let count = ref 0 in
    for i=0 to (E.to_enum E.last) do
      if ((1 lsl i) land v) <> 0 then incr count;
    done;
    !count

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

    (* raises exception on empty *)
  let pop v =
    let elem = find v (fun _ -> true) in
    let v = remove v elem in
    elem, v

  let pop_opt v =
    try
      Some(pop v)
    with Not_found -> None

  let pp fmt v =
    Format.fprintf fmt "%d" v

end
