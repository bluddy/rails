  open! Containers
  open Ppx_yojson_conv_lib.Yojson_conv.Primitives
  module A = Bigarray.Array1

(* Tilebuffer: buffer for keeping track of tiles and boxes on screen
   Built for high performance
 *)

  type t = {
    width: int;
    buffer: int array;
  } [@@deriving yojson]

  let create width height =
    {
      width=width;
      buffer=Array.make (width * height) 0
    }

  let calc_offset v x y = 
    y * v.width + x

  let get v x y = v.buffer.(calc_offset v x y)

  let get_loc v x y =
    (* Gets us locations on-screen *)
    try
      let d = get v x y in
      if d > 0 then Some (d mod v.width, d / v.width)
      else None
    with
      Invalid_argument _ -> invalid_arg @@ Printf.sprintf "out of bounds for (%d,%d)" x y

  let set v x y ~value = v.buffer.(calc_offset v x y) <- value

  let is_empty_box v x y ~w ~h =
    let exception Stop in
    try
      for i=y to y+h-1 do 
        for j=x to x+w-1 do
          if get v j i <> 0 then raise Stop
        done
      done;
      true
    with
    | Stop -> false

  let set_box v ~x ~y ~w ~h value_x value_y =
    (* Don't mark the whole box - just the corners *)
    let value = value_y * v.width + value_x in
    for i=y to y+h-1 do
      for j=x to x+w-1 do
        set v j i ~value
      done
    done

  let clear v = Array.fill v.buffer 0 (Array.length v.buffer) 0

