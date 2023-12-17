  open! Containers
  module A = Bigarray.Array1

(* Tilebuffer: buffer for keeping track of tiles on screen
   Only a single bool can be stored at each point
 *)

  type t = {
    width: int;
    buffer: (int, Bigarray.int_elt, Bigarray.c_layout) A.t; 
  }

  let create width height =
    {
      width=width;
      buffer=A.(init Int C_layout (width * height) (fun _ -> 0))
    }

  let calc_offset v x y = 
    y * v.width + x

  let get v x y =
    A.get v.buffer (calc_offset v x y)

  let get_loc v x y =
    let d = get v x y in
    if d > 0 then Some (d mod v.width, d / v.width)
    else None

  let set v x y ~value =
    A.set v.buffer (calc_offset v x y) value

  (* let reset v x y = *)
  (*   A.fill v.buffer 0 *)

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

  let clear v =
    A.fill v.buffer 0

  let t_of_yojson x = match x with
    | `Tuple [`Int w; `Int h] -> create w h
    | _ -> invalid_arg "Bad json values"

  let yojson_of_t v =
    let w = v.width in
    let h = (A.dim v.buffer) / w in
    `Tuple [`Int w; `Int h]



