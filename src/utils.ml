open Containers
let clip v ~min ~max =
  if v >= min then 
    if v <= max then
      v
    else
      max
  else
    min

    (* A swirl of offsets going clockwise starting from closest point
       going outwards: 3x3, 5x5 up to 7x7.
       The corners are skipped and are at the end.
       Length: 48.
     *)
let x_offset = 
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

let y_offset =
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

let random_int maxval r =
  if maxval = 0 then 0 else Random.int maxval r
  
