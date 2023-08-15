open Containers

type t =
  | Up
  | UpRight
  | Right
  | DownRight
  | Down
  | DownLeft
  | Left
  | UpLeft
  [@@deriving show, eq, ord, enum, yojson]

let (=) x y = equal x y
let (<>) x y = not(equal x y)
let (<) x y = compare x y < 0
let (>) x y = compare x y > 0
let (>=) x y = compare x y >= 0
let (<=) x y = compare x y <= 0

let dirlist = [Up; UpRight; Right; DownRight; Down; DownLeft; Left; UpLeft]
let dirlist_left = [Left; UpLeft; Up; UpRight; Right; DownRight; Down; DownLeft]

let cw = function
  | Up -> UpRight
  | UpRight -> Right
  | Right -> DownRight
  | DownRight -> Down
  | Down -> DownLeft
  | DownLeft -> Left
  | Left -> UpLeft
  | UpLeft -> Up

let ccw = function
  | Up -> UpLeft
  | UpRight -> Up
  | Right -> UpRight
  | DownRight -> Right
  | Down -> DownRight
  | DownLeft -> Down
  | Left -> DownLeft
  | UpLeft -> Left

let opposite = function
  | Up -> Down
  | UpRight -> DownLeft
  | Right -> Left
  | DownRight -> UpLeft
  | Down -> Up
  | DownLeft -> UpRight
  | Left -> Right
  | UpLeft -> DownRight

let within_90 dir dir2 =
  let dir = to_enum dir in
  let dir2 = to_enum dir2 in
  let diff = dir2 - dir in
  let diff = if diff < 0 then diff + 8 else diff in
  diff <= 2

let is_cardinal = function
  | Up
  | Right
  | Left
  | Down -> true
  | _ -> false

let is_diagonal = function
  | UpRight
  | UpLeft
  | DownRight
  | DownLeft -> true
  | _ -> false

let diff dir1 dir2 =
  let diff = abs(to_enum dir1 - to_enum dir2) in
  if diff > 4 then 8 - diff else diff

module Set = struct
  include Bitset.Make(struct
    type nonrec t=t
    let to_enum = to_enum
    let of_enum = of_enum
    let last = UpLeft
    let yojson_of_t = yojson_of_t
    let t_of_yojson = t_of_yojson
  end)

  (* Convert bool mask to dir set *)
  let of_mask mask =
    Iter.foldi (fun acc i v ->
      if v then add acc (of_enum i |> Option.get_exn_or "dir")
      else acc)
    empty
    mask

  (* Number of near options, including original dir *)
  let num_adjacent dir dirs =
    let ret = 0 in
    let ret = if mem dirs dir then ret + 1 else ret in
    let ret = if mem dirs (cw dir) then ret + 1 else ret in
    let ret = if mem dirs (ccw dir) then ret + 1 else ret in
    ret

  (* Find the closest dir to the original direction,
      allowing up to a 90 degree turn
    *)
  let find_nearest dir dirs =
    let check = mem dirs in
    if check dir then Some dir else
    let cwd = cw dir in
    if check cwd then Some cwd else
    let ccwd = ccw dir in
    if check ccwd then Some ccwd else
    let cwd = cw cwd in
    if check cwd then Some cwd else
    let ccwd = ccw ccwd in
    if check ccwd then Some ccwd else
    None
end


(* A swirl of offsets going clockwise starting from closest point
    going outwards: 3x3, 5x5 up to 7x7.
    The corners are skipped and are at the end.
    Length: 48.

   47 42 43 24 25 26 44
   41 22 23 08 09 10 27
   40 21 07 00 01 11 28
   39 20 06 XX 02 12 29
   38 19 05 04 03 13 30
   37 18 17 16 15 14 31
   46 36 35 34 33 32 45
*)
let y_offset = 
  [|-1;   -1;   0;   1;
     1;    1;   0;  -1;
    -2;   -2;  -2;  -1;
     0;    1;   2;   2;
     2;    2;   2;   1;
     0;   -1;  -2;  -2;
    -3;   -3;  -3;  -2;
    -1;    0;   1;   2;
     3;    3;   3;   3;
     3;    2;   1;   0;
    -1;   -2;  -3;  -3;
     3;   -3;   3;  -3;
  |]

let x_offset =
  [| 0;  1;  1;  1;
      0; -1; -1; -1;
      0;  1;  2;  2;
      2;  2;  2;  1;
      0; -1; -2; -2;
      -2; -2; -2; -1;
      0;  1;  2;  3;
      3;  3;  3;  3;
      2;  1;  0; -1;
      -2; -3; -3; -3;
      -3; -3; -2; -1;
      3;  3; -3; -3;
  |]

let to_offsets dir =
  let i = to_enum dir in
  x_offset.(i), y_offset.(i)

let adjust dir x y =
  let dx, dy = to_offsets dir in
  x + dx, y + dy

  (* 
     Convolution operator for height 
     1 2 3 2 1
     2 4 6 4 2
     3 6 X 6 3
     2 4 6 4 2
     1 2 3 2 1

  *)
let offset_conv_map =
  [6; 4; 6; 4; 6; 4; 6; 4; (* direct dir *)
   3; 2; 1; (* top *)
   2; 3; 2; 1; (* right *)
   2; 3; 2; 1; (* bottom *)
   2; 3; 2; 1; (* left *)
   2; (* top *)
  ]

