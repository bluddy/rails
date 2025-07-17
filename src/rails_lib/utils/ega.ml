open Ppx_yojson_conv_lib.Yojson_conv.Primitives


(* This is the standard EGA palette.
   For transparent images, 0 is used as alpha=0, and 5 is used for black
   i.e. alpha images can't have magenta
 *)

let palette =
  [|
    0x0;      (* 0, black/transparent, magenta *)
    0xAA;     (* 1, blue *)
    0xAA00;   (* 2, green *)
    0xAAAA;   (* 3, cyan *)
    0xAA0000; (* 4, red *)
    0xAA00AA; (* 5, black *)
    0xAA5500; (* 6, brown *)
    0xAAAAAA; (* 7, grey *)
    0x555555; (* 8, dgray *)
    0x5555FF; (* 9, bblue *)
    0x55FF55; (* a, bgreen *)
    0x55ffff; (* b, bcyan *)
    0xff5555; (* c, bred *)
    0xff55ff; (* d, bmagenta *)
    0xffff55; (* e, yellow *)
    0xffffff; (* f, white *)
  |]

  (* returns color and opacity
     debug: view transparency as magenta
   *)
let get_color ~transparent ?(debug=false) i =
  if i >= Array.length palette then
    failwith @@ Printf.sprintf "Ega.get_color: Color %d is invalid" i
  else
    match i, transparent, debug with
    | 0, true, true -> palette.(5), 0xFF
    | 0, true, false -> palette.(i), 0x0
    | 5, true, _ -> palette.(0), 0xFF
    | _ -> palette.(i), 0xFF

(* These colors are only for drawing, i.e. not transparent *)

let get_rgba ?debug i =
  let color, a = get_color ~transparent:false ?debug i in
  (color lsr 16, (color lsr 8) land 0xFF, color land 0xFF, a)

let get_rgb ?debug i =
  let color, _ = get_color ~transparent:false ?debug i in
  (color lsr 16, (color lsr 8) land 0xFF, color land 0xFF)

type color = int * int * int * int [@@deriving eq, yojson]

let black = get_rgba 0
let blue = get_rgba 1
let green = get_rgba 2
let cyan = get_rgba 3
let red = get_rgba 4
let magenta = get_rgba 5
let brown = get_rgba 6
let gray = get_rgba 7
let dgray = get_rgba 8
let bblue = get_rgba 9
let bgreen = get_rgba 10
let bcyan = get_rgba 11
let bcyan_transparent =
  let r, g, b, _ = get_rgba 11 in
  r, g, b, 0xA0 (* half transparent *)
let bred = get_rgba 12
let bmagenta = get_rgba 13
let yellow = get_rgba 14
let white = get_rgba 15



