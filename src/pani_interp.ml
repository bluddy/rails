open Containers

(* Pani Interpreter: interprets the language of the PANI file *)
(* Notes: pay attention to signed vs unsigned comparisons *)

let debug = true

module Animation = struct

type t = {
  mutable used: bool;
  mutable active: bool;
  other_anim_idx: int;
  x_diff: int;
  y_diff: int;
  x: int;
  y: int;
  delay_reset: int;
  delay: int;
  total_delay: int;
  timer_array: int Array.t;
  timer_idx: int;
  data_ptr_reset: int;
  data_ptr: int;
  far_ptr_flag: bool;
  data_size: int;
  pic_idx: int;
} 
[@@deriving show]

let empty () = {
  used=false; active=false; other_anim_idx=0; x_diff=0; y_diff=0; x=0; y=0;
  delay_reset=0; delay=0; total_delay=255; timer_array = Array.make 10 0; timer_idx=0;
  data_ptr_reset=0; data_ptr=0; far_ptr_flag=false; data_size=0; pic_idx=0;
}

let create ~data_ptr ~other_anim_idx ~x_diff ~y_diff ~delay ~pic_far =
  let a = empty () in
  let x, y =
    if other_anim_idx + 1 = 0 then x_diff, y_diff else 0, 0
  in
  {a with
    used=true;
    active=false;
    data_ptr_reset=data_ptr;
    data_ptr;
    other_anim_idx=other_anim_idx-1;
    x_diff; y_diff;
    x; y;
    delay_reset=delay;
    delay;
    far_ptr_flag=pic_far;
  }

end

type t = {
  mutable error: bool;
  mutable do_timeout: bool;
  mutable timeout: int;
  buffer: bytes;
  mutable read_ptr: int;
  mutable stack: int list;
  pani_array: int Array.t;
  animations: Animation.t Array.t;
}

let create s =
{
  error=false;
  do_timeout=false;
  timeout=0;
  read_ptr=0;
  buffer=s;
  stack=[];
  pani_array=Array.make 52 0;
  animations=Array.init 50 (fun _ -> Animation.empty ());
}

type op =
  | CreateAnimation
  | DeleteAnimation
  | SetTimeout
  | DebugWrite
  | ActivateAnimation
  | TimeoutPush
  | SetTimeoutWriteAnimArray
  | Copy
  | Eq
  | Neq
  | Gt
  | Lt
  | Geq
  | Leq
  | Add
  | Sub
  | Mult
  | Div
  | JumpIfTrue
  | Jump
  | SetError
  | CallFunc
  | Return
  | Exit
  [@@deriving show]

let op_of_byte = function
  | 0 -> CreateAnimation
  | 1 -> DeleteAnimation
  | 2 -> SetTimeout
  | 3 -> DebugWrite
  | 4 -> ActivateAnimation
  | 5 -> TimeoutPush
  | 6 -> SetTimeoutWriteAnimArray
  | 7 -> Copy
  | 8 -> Eq
  | 9 -> Neq
  | 10 -> Gt
  | 11 -> Lt
  | 12 -> Geq
  | 13 -> Leq
  | 14 -> Add
  | 15 -> Sub
  | 16 -> Mult
  | 17 -> Div
  | 18 -> JumpIfTrue
  | 19 -> Jump
  | 20 -> Exit
  | 21 -> SetError
  | 22 -> Return
  | 23 -> CallFunc
  | x  -> failwith @@ Printf.sprintf "Unknown op code %d" x

let str_of_stack v =
  "[" ^ (String.concat ", " @@ List.map Int.to_string v.stack) ^ "]"

let read_byte v =
  let ptr = v.read_ptr in
  v.read_ptr <- v.read_ptr + 1;
  Bytes.get_int8 v.buffer ptr

let read_word v =
  let ptr = v.read_ptr in
  v.read_ptr <- v.read_ptr + 2;
  Bytes.get_int16_le v.buffer ptr

let is_true x = x <> 0

let interpret v =
  let byte = read_byte v in
  let op = op_of_byte byte in
  if debug then begin
    Printf.printf "%s(0x%x) stack: %s\n" (show_op op) byte (str_of_stack v);
  end;
  match op with
  | Add | Sub | Mult | Div
  | Eq | Neq | Gt | Lt | Geq | Leq ->
      let g f x y = if f x y then 1 else 0 in
      let f = match op with
        | Add  -> (+)
        | Sub  -> (-)
        | Mult -> ( * )
        | Div  -> (/)
        | Eq   -> g (=)
        | Neq  -> g (<>)
        | Gt   -> g (>)
        | Lt   -> g (<) 
        | Geq  -> g (>=)
        | Leq  -> g (<=)
        | _ -> assert false
      in
      let stack' =
        match v.stack with
        | x::y::z ->
            let result = f y x in
            result::z
        | _ -> failwith "Cannot add. Stack has < 2 elements"
      in
      v.stack <- stack';
      true
  | CreateAnimation ->
      begin match v.stack with
      | pic_far::delay::y_diff::x_diff::other_anim_idx::anim_idx::data_ptr::rest -> 
        let anim_idx =
          if anim_idx = 0xFFFF then begin
            Printf.printf "call find_unused_anim\n";
            0
          end else anim_idx
        in
        if anim_idx > 0 && anim_idx <= 50 then begin
          let anim = 
            let pic_far = pic_far = 1 in
            Animation.create ~pic_far ~delay ~y_diff ~x_diff ~other_anim_idx ~data_ptr
          in
          if debug then
            Printf.printf "Animation %d:\n%s\n" anim_idx (Animation.show anim);
          v.animations.(anim_idx) <- anim
        end;
        v.stack <- rest
      | _ -> failwith "Invalid stack for animation creation"
      end;
      true
  | DeleteAnimation ->
      begin match v.stack with
      | anim_idx::rest ->
          if anim_idx > 0 && anim_idx <= 50 then begin
            v.animations.(anim_idx).used <- false
          end;
          v.stack <- rest
      | _ -> failwith "DeleteAnimation: missing anim_idx on stack"
      end;
      true
  | SetTimeout ->
      begin match v.stack with
      | timeout :: rest ->
          v.do_timeout <- true;
          v.timeout <- timeout;
          v.stack <- rest
      | _ -> failwith "SetTimeout: missing timeout argument"
      end;
      true
  | DebugWrite ->
      begin match v.stack with
      | _::rest ->
          Printf.printf "do debug_write";
          v.stack <- rest
      | _ -> failwith "DebugWrite: missing value argument"
      end;
      true
  | ActivateAnimation ->
      begin match v.stack with
      | anim_idx::rest ->
          if anim_idx > 0 && anim_idx <= 50 then begin
            v.animations.(anim_idx).active <- true
          end;
          v.stack <- rest
      | _ -> failwith "ActivateAnimation: missing argument"
      end;
      true
  | TimeoutPush ->
      let test = read_byte v in
      let timeout = read_word v in
      begin if is_true test then 
        v.timeout <- v.pani_array.(timeout)
      else
        v.timeout <- timeout
      end;
      v.stack <- v.timeout::v.stack;
      true
  | SetTimeoutWriteAnimArray ->
      begin match v.stack with
      | newval::rest ->
        let timeout = read_word v in
        v.timeout <- timeout;
        if timeout > 0 && timeout <= 50 then begin
          v.pani_array.(timeout) <- newval
        end;
        v.stack <- rest
      | _ -> failwith "SetTimeoutWriteAnimArray: missing argument"
      end;
      true
  | Copy ->
      begin match v.stack with
      | x::rest ->
        v.stack <- x::x::rest
      | _ -> failwith "Copy: missing argument"
      end;
      true
  | JumpIfTrue ->
      begin match v.stack with
      | do_jump::rest ->
          let addr = read_word v in
          if is_true do_jump then begin
            v.read_ptr <- addr
          end;
          v.stack <- rest
      | _ -> failwith "JumpIfTrue: missing argument"
      end;
      true
  | Jump ->
      let addr = read_word v in
      v.read_ptr <- addr;
      true
  | SetError ->
      v.error <- true;
      true
  | Exit ->
      false
  | CallFunc ->
      let jump_addr = read_word v in
      v.stack <- v.read_ptr :: v.stack;
      v.read_ptr <- jump_addr;
      true
  | Return ->
      begin match v.stack with
      | ret_addr::rest ->
        v.read_ptr <- ret_addr;
        v.stack <- rest
      | _ -> failwith "Return: missing return address"
      end;
      true

let run str =
  let v = create str in
  let rec loop () =
    if v.error then ()
    else begin
      if v.do_timeout then begin
        v.timeout <- v.timeout - 1;
        if v.timeout = 0 then begin
          v.do_timeout <- false;
          loop ()
        end
      end;
      if interpret v then loop ()
      else ()
    end
  in
  loop ()

