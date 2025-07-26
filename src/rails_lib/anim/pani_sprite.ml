open Containers

(* Pani animations: interpret the code of the individual animations *)

let debug = ref false

let print_hex fmt = Format.fprintf fmt "0x%x"

type t = {
  mutable active: bool;     (* not active: deleted *)
  mutable visible: bool;
  mutable read_ptr: int   [@printer print_hex];  (* offset into buffer *)
  mutable pic_idx: int;   (* -1: no pic *)
  mutable x: int;
  mutable y: int;
  mutable counter_stack: int list;
  other_anim_idx: int;
  reset_x: int;
  reset_y: int;
  reset_delay: int;
  mutable delay: int;
  mutable total_delay: int;
  reset_read_ptr: int    [@printer print_hex];
  data_size: int;
  buffer: bytes; [@opaque]
} 
[@@deriving show]

type op =
  | SetPicIdx
  | SetXY
  | AddXY
  | SetDelay
    (* Set a delay amount to add to total_delay each call *)
  | AddDelay
  | PushCounter
    (* Push a counter at the top of the counter stack *)
  | JumpN
    (* Jump N iterations, based on top of counter stack *)
  | Reset
    (* Reset all *)
  | ResetReadPtr
  | Pause
  | Deactivate
  [@@deriving show]

let op_of_byte ?(idx=0) = function
  | 0 -> SetPicIdx
  | 1 -> SetXY
  | 2 -> AddXY
  | 3 -> SetDelay
  | 4 -> AddDelay
  | 5 -> PushCounter
  | 6 -> JumpN
  | 7 -> Reset
  | 8 -> ResetReadPtr
  | 9 -> Pause
  | 10 -> Deactivate
  | x -> failwith @@ Printf.sprintf "anim[%d]: Unsupported byte %d" idx x

let empty () = {
  active=false;
  visible=false;
  other_anim_idx=0;
  reset_x=0;
  reset_y=0;
  x=0;
  y=0;
  reset_delay=0;
  delay=0;
  total_delay=255;
  counter_stack=[];
  reset_read_ptr=0;
  read_ptr=0;
  data_size=0;
  pic_idx=0;
  buffer=Bytes.empty
}

let make ~data_ptr ~other_anim_idx ~reset_x ~reset_y ~delay ~pic_far ~buffer =
  let _f = pic_far in
  let a = empty () in
  let other_anim_idx = other_anim_idx - 1 in
  let x, y =
    (* Independent animations reset to the reset values. 
       Dependent animations reset to 0 *)
    if other_anim_idx = -2
    then reset_x, reset_y
    else 0, 0
  in
  {a with
    active=true;
    visible=false;
    reset_read_ptr=data_ptr;
    read_ptr=data_ptr;
    other_anim_idx;
    reset_x; reset_y;
    x; y;
    reset_delay=delay;
    delay;
    buffer;
  }

let read_byte v =
  let ptr = v.read_ptr in
  v.read_ptr <- v.read_ptr + 1;
  Bytes.get_int8 v.buffer ptr

  (* move pointer back one *)
let reset_byte_loc v =
  v.read_ptr <- v.read_ptr - 1

let reset_byte_to_zero v =
  reset_byte_loc v;
  Bytes.set_int8 v.buffer v.read_ptr 0

let read_word v =
  let ptr = v.read_ptr in
  v.read_ptr <- v.read_ptr + 2;
  Bytes.get_int16_le v.buffer ptr

let interpret_step v idx =
  if v.active then begin
    v.total_delay <- v.total_delay + v.delay;

    if v.total_delay > 255 then begin
      v.total_delay <- v.total_delay - 255;
      let rec interp_loop () =
        let byte = read_byte v in
        let op = op_of_byte byte ~idx in

        if !debug then
          Printf.printf "anim[%d] 0x%x: %s(0x%x)\n" idx (v.read_ptr-1) (show_op op) byte;

        let action = match op with
          | SetPicIdx ->
              let pic_idx = read_byte v in
              v.pic_idx <- pic_idx;
              `Exit
          | SetXY ->
              let x = read_word v in
              let y = read_word v in
              v.x <- x;
              v.y <- y;
              `Stay
          | AddXY ->
              let x = read_word v in
              let y = read_word v in
              v.x <- v.x + x;
              v.y <- v.y + y;
              `Stay
          | SetDelay ->
              let delay = read_byte v in
              v.delay <- delay;
              `Stay
          | AddDelay ->
              let delay = read_byte v in
              v.delay <- v.delay + delay;
              `Stay
          | PushCounter ->
              let counter = read_word v in
              v.counter_stack <- counter::v.counter_stack;
              `Stay
          | JumpN ->
              begin match v.counter_stack with
              | counter::rest when counter = 1 ->
                  (* Must dispose of bytes *)
                  let _ = read_word v in
                  v.counter_stack <- rest;
              | counter::rest ->
                  v.counter_stack <- (counter-1)::rest;
                  (* JumpN *)
                  let addr = read_word v in
                  v.read_ptr <- addr
              | _ -> failwith "JumpN: missing counter value on stack"
              end;
              `Stay
          | Reset ->
              if v.other_anim_idx = -2 then (
                v.x <- v.reset_x;
                v.y <- v.reset_y
              ) else (
                v.x <- 0;
                v.y <- 0
              );
              v.delay <- v.reset_delay;
              v.total_delay <- 255;
              v.counter_stack <- [];
              v.read_ptr <- v.reset_read_ptr;
              `Stay
          | ResetReadPtr ->
              v.counter_stack <- [];
              v.read_ptr <- v.reset_read_ptr;
              `Stay
          | Pause ->
              v.read_ptr <- v.read_ptr - 1;
              `Exit
          | Deactivate ->
              v.read_ptr <- v.read_ptr - 1;
              v.active <- false;
              `Destroy
        in
        if !debug then print_endline @@ show v;
        match action with
        | `Stay -> interp_loop ()
        | `Exit -> `None
        | `Destroy -> `Destroy
      in
      interp_loop ()
    end
    else `None
  end
  else `None

