open Containers
open Tsdl
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

module CharMap = Map.Make(struct type t = char let compare x y = Char.to_int x - Char.to_int y end) 

module type OrderedType = sig
  include Map.OrderedType
  val t_of_yojson : Yojson.Safe.t -> t
  val yojson_of_t : t -> Yojson.Safe.t
end

module Infix = struct
  let (===) = Stdlib.(==)
  let (=!=) = Stdlib.(!=)
  type rw = [`RW] [@@deriving yojson]
  type ro = [`RO] [@@deriving yojson]
end

module Random = struct
  (* Expand Random to serialize the state *)
  include Random
  module State = struct
    include Random.State
    let t_of_yojson = function
    | `String s -> Marshal.from_string s 0
    | _ -> failwith "unexpected json"
    let yojson_of_t v =
      `String (Marshal.to_string v [])
  end
end

(* Set with sexplib extension *)
module Set = struct
  module type S = sig
    include CCSet.S
    val t_of_yojson : Yojson.Safe.t -> t
    val yojson_of_t : t -> Yojson.Safe.t
  end
  module Make(O:OrderedType) = struct
    include CCSet.Make(O)

    let t_of_yojson (json:Yojson.Safe.t) =
      list_of_yojson O.t_of_yojson json |> of_list

    let yojson_of_t (t:t) =
      to_list t |> yojson_of_list O.yojson_of_t
  end
end

module Map = struct
  module type S = sig
    include CCMap.S
    val t_of_yojson : Yojson.Safe.t -> 'a t
    val yojson_of_t : 'a t -> Yojson.Safe.t
  end
  module Make(O:OrderedType) = struct
    include CCMap.Make(O)

    let to_hashtbl v =
      let h = Hashtbl.create 10 in
      iter (fun k v -> Hashtbl.replace h k v) v;
      h

    let of_hashtbl h =
      let m = empty in
      Hashtbl.fold (fun k v m -> add k v m) h m

    let t_of_yojson conv (json:Yojson.Safe.t) =
      hashtbl_of_yojson O.t_of_yojson conv json |> of_hashtbl

    let yojson_of_t conv v =
      to_hashtbl v |> yojson_of_hashtbl O.yojson_of_t conv
  end
end

module IntMap = Map.Make(struct
  type t = int [@@deriving yojson]
  let compare (x:int) y = x - y
end)

type loc = int * int
  [@@deriving eq, ord, yojson, show]

type locd = loc * Dir.t
  [@@deriving eq, ord, yojson, show]

type locu = loc * Dir.upper
  [@@deriving eq, ord, yojson, show]

let locu_of_locd (loc, d) = (loc, Dir.to_upper d)

type locdpair = locd * locd
  [@@deriving eq, ord, yojson, show]

(* Expand CCHashSet *)
module type S2 = sig
  include CCHashSet.S
  val choose: t -> elt option
  val choose_exn: t -> elt
  val is_empty: t -> bool
end

module HashSet = struct
  module Make(E: CCHashSet.ELEMENT) : S2 with type elt = E.t = struct
    include CCHashSet.Make(E)
    let choose v = to_iter v |> Iter.head
    let choose_exn v = to_iter v |> Iter.head_exn
    let is_empty v = cardinal v = 0
  end
end

module LocSet = HashSet.Make(struct
  type t = loc [@@deriving yojson]
  let equal = equal_loc
  let hash = Hashtbl.hash
end)

module LocdSet = HashSet.Make(struct
  type t = locd [@@deriving yojson]
  let equal = equal_locd
  let hash = Hashtbl.hash
end)

module LocuSet = HashSet.Make(struct
  type t = locu [@@deriving yojson]
  let equal = equal_locu
  let hash = Hashtbl.hash
end)

  (* A canonical order for locdp *)
let canonical_locdpair ((locd1, locd2) as p) =
   if compare_locd locd1 locd2 > 0 then locd2, locd1 else p

module IntIntMap = Map.Make(struct
  type t = loc [@@deriving yojson]
  let compare = compare_loc
end)

module Hashtbl = struct
  include Hashtbl

  let t_of_yojson conva convb json =
    hashtbl_of_yojson conva convb json

  let yojson_of_t conva convb t =
    yojson_of_hashtbl conva convb t
end

module Vector = struct
  open Infix
  include CCVector

  let foldi f acc (v:'a vector) =
    let rec foldi acc i =
      if i = length v then
        acc
      else (
        let x = Array.unsafe_get (unsafe_get_array v) i in
        foldi (f i acc x) (i + 1)
      )
    in
    foldi acc 0

  let mapi_in_place f v =
    for i=0 to (length v) - 1 do
      let x = get v i in
      let x' = f i x in
      if x =!= x' then (
        set v i x'
      );
    done

  let modify_at_idx (v:'a vector) i f =
    let x = get v i in
    let x' = f x in
    if x =!= x' then (
      set v i x'
    )

  let rw_of_yojson _ = `RW
  let yojson_of_rw _ = `Null
  
  let vector_of_yojson conv (json:Yojson.Safe.t) =
    list_of_yojson conv json |> CCVector.of_list

  let yojson_of_vector conv v =
    CCVector.to_list v |> yojson_of_list conv

  (* let pp_rw _ _ _ = () *)

  let pp_vector ?pp_start ?pp_stop ?pp_sep pp_item fmt (v:'a vector) =
    pp ?pp_start ?pp_stop ?pp_sep pp_item fmt v

end

type rect = {
  x: int;
  y: int;
  w: int;
  h: int;
} [@@deriving yojson]

type point = {
  x: int;
  y: int;
}

type msg = {
  x: int;
  y: int;
  dir: Dir.t;
  player: int;
} [@@deriving show]

let clip v ~min ~max =
  if v >= min then 
    if v <= max then
      v
    else
      max
  else
    min

let random_int maxval r =
  if maxval = 0 then 0 else Random.int maxval r
  
let calc_offset width x y = y * width + x

let x_y_of_offset width offset =
  let y = offset / width in
  let x = offset mod width in
  x, y

let eq_xy (x1,y1) (x2,y2) = x1 = x2 && y1 = y2

let neq_xy x y = not @@ eq_xy x y

module List = struct
  include List

  let findi f l =
    let rec loop i = function
      | x::_ when f i x -> Some x
      | _::xs -> loop (i+1) xs
      | _ -> None
    in
    loop 0 l
        
  let modify_at_idx i f l0 =
    let rec loop i acc = function
      | [] -> l0
      | y::ys when i=0 ->
          List.rev_append acc ((f y) :: ys)
      | y::ys ->
          loop (i-1) (y::acc) ys
    in
    loop i [] l0

  let modify_make_at_idx i f l0 =
    let rec loop i acc = function
      | [] -> l0, None
      | y::ys when i=0 ->
          let y', prod = f y in
          List.rev_append acc (y' :: ys), Some prod
      | y::ys ->
          loop (i-1) (y::acc) ys
    in
    loop i [] l0
end

let fold_range ~range ~x ~y ~width ~height ~read_f ~f ~init =
  let min_x = max 0 (x-range) in
  let max_x = min (width-1) (x+range) in
  let min_y = max 0 (y-range) in
  let max_y = min (height-1) (y+range) in
  let v = ref init in
  for i=min_y to max_y do
    for j=min_x to max_x do
      let read_val = read_f j i in
      v := f !v j i read_val
    done
  done;
  !v

let scan_unordered ~range ~x ~y ~width ~height ~f =
  let min_x = max 0 (x-range) in
  let max_x = min (width-1) (x+range) in
  let min_y = max 0 (y-range) in
  let max_y = min (height-1) (y+range) in
  let exception Found of (int * int) option in
  try
    for i=min_y to max_y do
      for j=min_x to max_x do
        if f j i then
          raise_notrace @@ Found (Some (j, i));
      done
    done;
    None
  with
  | Found x -> x

  (* Scan in a spiral pattern *)
let scan =
  let offsets =
    [Dir.Right; Down; Left; Up] |> List.map Dir.to_offsets
  in
  let inner ~range ~x ~y ~width ~height ~f =
    if f x y then Some (x, y)
    else
      let rec loop i =
        if i > range then None
        else
          let start_x, start_y = x - i, y - i in

          let rec loop_inner counter x y lst = match lst with
            | (x_offset, y_offset)::_ when counter > 0 ->
                let x, y = x + x_offset, y + y_offset in
                if x >= 0 && y >= 0 && x < width && y < height && f x y then Some (x, y)
                else
                  loop_inner (counter - 1) x y lst
            | _::rest ->
                loop_inner (i*2) x y rest
            | [] ->
                None
          in
          match loop_inner (i*2) start_x start_y offsets with
          | None -> loop (i+1)
          | x -> x
      in
      loop 1
  in
  inner

let id x = x

let snd_option (x,y) =
  x, (y |> Option.get_exn_or "error")

let get_time () = Sdl.get_ticks () |> Int32.to_int

let string_of_num num =
  Printf.sprintf "%#d" num
  |> String.map (function '_' -> ',' | x -> x)

let dxdy (x1,y1) (x2,y2) =
  abs(x1-x2), abs(y1-y2)

  (* The way the original game approximates distance *)
let classic_dist loc1 loc2 =
  let dx, dy = dxdy loc1 loc2 in
  let big, small = if dx > dy then dx, dy else dy, dx in
  big + small / 2


(* Find one element from the left list that doesn't exist in the right list *)
let diff1_l ~eq left right =
  List.find_map (fun x ->
    if List.mem ~eq x right then None else Some x)
  left

(* Find one member that a larger list has that a smaller one doesn't *)
let diff1 ~eq left right =
  match diff1_l ~eq left right with
  | None -> diff1_l ~eq right left
  | x -> x

(* Return a diff member and an intersection member, if available *)
let diff_inter1 ~eq left right =
  match diff1 ~eq left right with
  | Some diff ->
    let find_inter l = List.find_opt (fun x -> not @@ eq x diff) l in
    let inter = Option.or_lazy (find_inter left) ~else_:(fun () -> find_inter right) in
    begin match inter with
    | Some inter -> Some (diff, inter)
    | _ -> None
    end
  | None -> None

let sum l = List.fold_left (+) 0 l

let modulo x y = 
  let res = x mod y in
  if res >= 0 then res else res + y

let fst3 (x,_,_) = x
let snd3 (_,x,_) = x
let thd3 (_,_,x) = x

let show_cash ?(spaces=0) region money =
  let b = Buffer.create 20 in
  Buffer.add_char b (Region.money_symbol region);
  let money_s = Printf.sprintf "%#d" money
    |> String.map (function '_' -> ',' | x -> x)
  in
  let len = String.length money_s in
  for _=0 to spaces-1-len do
    Buffer.add_char b ' ';
  done;
  Buffer.add_string b money_s;
  Buffer.add_string b ",000";
  Buffer.contents b

let other_period = function
  | `First -> `Second
  | `Second -> `First

