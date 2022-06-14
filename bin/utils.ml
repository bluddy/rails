open Containers
open Sexplib.Std

module CharMap = Map.Make(struct type t = char let compare x y = Char.to_int x - Char.to_int y end) 

type rect = {
  x: int;
  y: int;
  w: int;
  h: int;
} [@@deriving sexp]

type point = {
  x: int;
  y: int;
}

type msg = {
  x: int;
  y: int;
  dir: Dir.t;
  player: int;
}

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


module List = struct
  include List
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

let scan ~range ~x ~y ~width ~height ~f =
  let offsets =
    [Dir.Right; Down; Left; Up] |> List.map Dir.to_offsets
  in
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


let snd_option (x,y) =
  x, (y |> Option.get_exn_or "error")
