
let magenta = 0xAA00AA

let palette =
  [|
    0x0;      (* 0, black/transparent, magenta *)
    0xAA;     (* 1, blue *)
    0xAA00;   (* 2, green *)
    0xAAAA;   (* 3, cyan *)
    0xAA0000; (* 4, red *)
    0x0;      (* 5, black *)
    0xAA5500; (* 6, brown *)
    0xAAAAAA; (* 7, grey *)
    0x555555; (* 8, dgray *)
    0x5555FF; (* 9, bblue *)
    0x55FF55; (* a, bgreen *)
    0x55ffff; (* b, bcyan *)
    0xff5555; (* c, bred *)
    0xff55ff; (* d, bmagenta *)
    0xffff55; (* e, byellow *)
    0xffffff; (* f, bwhite *)
  |]

  (* returns color and opacity
     debug: view transparency as magenta
   *)
let get_color ?(debug=false) i =
  if i >= Array.length palette then
    failwith @@ Printf.sprintf "Ega.get_color: Color %d is invalid" i
  else
    match i with
    | 0 -> (if debug then magenta else palette.(i)), 0x0
    | _ -> palette.(i), 0xFF

let get_rgba ?debug i =
  let color, a = get_color ?debug i in
  (color lsr 16, (color lsr 8) land 0xFF, color land 0xFF, a)

let get_rgb ?debug i =
  let color, _ = get_color ?debug i in
  (color lsr 16, (color lsr 8) land 0xFF, color land 0xFF)

