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

let opposite = function
  | Up -> Down
  | UpRight -> DownLeft
  | Right -> Left
  | DownRight -> UpLeft
  | Down -> Up
  | DownLeft -> UpRight
  | Left -> Right
  | UpLeft -> DownLeft

module Set = struct
  include Bitset.Make(struct
    type nonrec t=t
    let to_enum = to_enum
    let of_enum = of_enum
    let last = UpLeft
  end)

  (* Convert bool mask to dir set *)
  let of_mask mask =
    Iter.foldi (fun acc i v ->
      if v then add acc (of_enum i |> Option.get_exn_or "dir")
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

let track_dirs =
  [[Up]; [UpRight]; [Right]; [DownRight]; [Down]; [DownLeft]; [Left]; [UpLeft];
  [Up; DownRight]; [Up; Down]; [Up; DownLeft]; [Down; UpRight]; [UpRight; DownLeft]; [Left; UpRight];
    [Right; DownLeft]; [Left; Right];
  [Right; UpLeft]; [Left; DownRight]; [UpLeft; DownRight]; [Down; UpLeft]; [Up; Down; DownRight];
    [Up; Down; DownLeft]; [Up; Down; UpRight]; [Up; Down; UpLeft]; [Left; Right; UpRight];
    [Left; Right; DownRight]; [Left; Right; UpLeft]; [Left; Right; DownLeft]; [Down; DownLeft; UpRight];
    [Left; DownLeft; UpRight]; [Up; UpLeft; DownRight]; [Left; UpLeft; DownRight];
  [Down; UpLeft; DownRight]; [Right; UpLeft; DownRight]; [Up; DownLeft; UpRight]; [Right; UpRight; DownLeft]]

  (* Only the computer can make these *)
let illegal_track =
  [[Up; Down; Left; Right]; [UpRight; DownRight; DownLeft; UpLeft]]

let track_turns =
  [[Up; Right]; [Down; Right]; [Down; Left]; [Up; Left]; [UpRight; DownRight]; [DownLeft; DownRight];
    [UpLeft; DownLeft]; [UpLeft; UpRight]]



