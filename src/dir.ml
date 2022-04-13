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
  [@@deriving ord, enum]

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

module Set = struct
  include Set.Make(struct type nonrec t=t let compare = compare end)

  (* Convert bool mask to dir set *)
  let of_mask mask =
    Iter.foldi (fun acc i v ->
      if v then add (of_enum i |> Option.get_exn_or "Dir") acc
      else acc)
    empty
    mask
end


(* A swirl of offsets going clockwise starting from closest point
    going outwards: 3x3, 5x5 up to 7x7.
    The corners are skipped and are at the end.
    Length: 48.
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



