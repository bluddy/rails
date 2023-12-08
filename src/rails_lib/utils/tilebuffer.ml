  open! Containers

(* Tilebuffer: buffer for keeping track of tiles on screen
   Only a single bool can be stored at each point
 *)

  type t = {
    width: int;
    buffer: CCBV.t
  }

  let create width height =
    {
      width=width;
      buffer=CCBV.create ~size:(width * height) false
    }

  let calc_offset v x y = 
    y * v.width + x

  let get v x y =
    CCBV.get v.buffer (calc_offset v x y)

  let set v x y =
    CCBV.set v.buffer (calc_offset v x y)

  let reset v x y =
    CCBV.reset v.buffer (calc_offset v x y)

  let is_empty_box v x y ~w ~h =
    let exception Stop in
    try
      for i=y to y+h-1 do 
        for j=x to x+w-1 do
          if get v j i then raise Stop
        done
      done;
      true
    with
    | Stop -> false

  let set_box v x y ~w ~h =
    (* Don't mark the whole box - just the corners *)
    set v x y;
    set v (x + w - 1) y;
    set v x (y + h - 1);
    set v (x + w - 1) (y + h - 1)


  let clear v =
    CCBV.clear v.buffer

  let t_of_yojson x = match x with
    | `Tuple [`Int w; `Int h] -> create w h
    | _ -> invalid_arg "Bad json values"

  let yojson_of_t v =
    let w = v.width in
    let h = (CCBV.length v.buffer) / w in
    `Tuple [`Int w; `Int h]



