
let palette =
  [|
    0x0;      (* 0, black *)
    0xAA;     (* 1, blue *)
    0xAA00;   (* 2, green *)
    0xAAAA;   (* 3, cyan *)
    0xAA0000; (* 4, red *)
    (* 0x0;      (* 5, black *) *)
    0xAA00AA; (* 5, magenta *)
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

let get_color i = palette.(i)
