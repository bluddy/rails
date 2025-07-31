open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

module UMap = Utils.Map

type t =
  | AI of int
  | Human of int
  [@@deriving yojson, eq, ord, show]

let (=) = equal
let (<>) x y = not (x = y) 

let last = ref 0

let create ~human () =
  let idx = !last in
  incr last;
  if human then Human idx else AI idx

let create_ai () = create ~human:false ()
let create_human () = create ~human:true ()
let is_human = function
  | Human _ -> true | _ -> false

let is_ai x = not @@ is_human x

module Map = UMap.Make(struct
  type nonrec t = t
  let compare = compare
  let yojson_of_t = yojson_of_t
  let t_of_yojson = t_of_yojson
end)
