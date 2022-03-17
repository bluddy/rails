open Containers

(* Pani Interpreter: interprets the language of the PANI file *)
(* Notes: pay attention to signed vs unsigned comparisons *)

let debug = true

type t = {
  mutable used: bool;
  mutable active: bool;
  other_anim_idx: int;
  reset_x: int;
  reset_y: int;
  mutable x: int;
  mutable y: int;
  reset_delay: int;
  mutable delay: int;
  mutable total_delay: int;
  mutable timer_stack: int list;
  reset_read_ptr: int;
  mutable read_ptr: int;
  far_ptr_flag: bool;
  data_size: int;
  mutable pic_idx: int;
  buffer: bytes; [@opaque]
} 
[@@deriving show]

type op =
  | SetPicIdx
  | SetXY
  | AddXY
  | SetDelay
  | AddDelay
  | SetTimer
  | Jump
    (* We can only jump based on timer stack *)
  | Reset
  | ResetReadPtr
  | Nop
  | Delete
  [@@deriving show]

let op_of_byte = function
  | 0 -> SetPicIdx
  | 1 -> SetXY
  | 2 -> AddXY
  | 3 -> SetDelay
  | 4 -> AddDelay
  | 5 -> SetTimer
  | 6 -> Jump
  | 7 -> Reset
  | 8 -> ResetReadPtr
  | 9 -> Nop
  | 10 -> Delete
  | x -> failwith @@ Printf.sprintf "Unsupported byte %d" x

let empty () = {
  used=false; active=false; other_anim_idx=0; reset_x=0; reset_y=0; x=0; y=0;
  reset_delay=0; delay=0; total_delay=255; timer_stack=[];
  reset_read_ptr=0; read_ptr=0; far_ptr_flag=false; data_size=0; pic_idx=0;
  buffer=Bytes.empty
}

let make ~data_ptr ~other_anim_idx ~reset_x ~reset_y ~delay ~pic_far ~buffer =
  let a = empty () in
  let x, y =
    if other_anim_idx + 1 = 0 then reset_x, reset_y else 0, 0
  in
  {a with
    used=true;
    active=false;
    reset_read_ptr=data_ptr;
    read_ptr=data_ptr;
    other_anim_idx=other_anim_idx-1;
    reset_x; reset_y;
    x; y;
    reset_delay=delay;
    delay;
    far_ptr_flag=pic_far;
    buffer;
  }

let read_byte v =
  let ptr = v.read_ptr in
  v.read_ptr <- v.read_ptr + 1;
  Bytes.get_int8 v.buffer ptr

let read_word v =
  let ptr = v.read_ptr in
  v.read_ptr <- v.read_ptr + 2;
  Bytes.get_int16_le v.buffer ptr

let interpret_step v idx =
  if not v.used then () else begin
    v.total_delay <- v.total_delay + v.delay;

    if v.total_delay > 255 then begin
      v.total_delay <- v.total_delay - 255;
      let byte = read_byte v in
      let op = op_of_byte byte in

      if debug then
        Printf.printf "anim[%d] 0x%x: %s(0x%x)\n" idx v.read_ptr (show_op op) byte;

      match op with
      | SetPicIdx ->
          let pic_idx = read_byte v in
          v.pic_idx <- pic_idx
      | SetXY ->
          let x = read_word v in
          let y = read_word v in
          v.x <- x;
          v.y <- y;
      | AddXY ->
          let x = read_word v in
          let y = read_word v in
          v.x <- v.x + x;
          v.y <- v.y + y
      | SetDelay ->
          let delay = read_byte v in
          v.delay <- delay
      | AddDelay ->
          let delay = read_byte v in
          v.delay <- v.delay + delay
      | SetTimer ->
          let timer = read_byte v in
          v.timer_stack <- timer::v.timer_stack
      | Jump ->
          begin match v.timer_stack with
          | timer::rest when timer = 1 ->
              v.timer_stack <- rest
          | timer::rest ->
              v.timer_stack <- (timer-1)::rest;
              (* Jump *)
              let addr = read_byte v in
              v.read_ptr <- addr
          | _ -> failwith "Jump: missing timer value on stack"
          end
      | Reset ->
          if v.other_anim_idx + 2 = 0 then begin
            v.x <- 0;
            v.y <- 0
          end else begin
            v.x <- v.reset_x;
            v.y <- v.reset_y
          end;
          v.delay <- v.reset_delay;
          v.total_delay <- 255;
          v.timer_stack <- [];
          v.read_ptr <- v.reset_read_ptr;
      | ResetReadPtr ->
          v.timer_stack <- [];
          v.read_ptr <- v.reset_read_ptr;
      | Nop -> ()
      | Delete ->
          v.used <- false
    end
    else ()
  end
