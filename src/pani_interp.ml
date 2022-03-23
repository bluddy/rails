open Containers

(* Pani Interpreter: interprets the language of the PANI file *)
(* Notes: pay attention to signed vs unsigned comparisons *)

module Ndarray = Owl_base_dense_ndarray.Generic

let debug = true

type t = {
  mutable is_done: bool;
  mutable timeout: bool;
  mutable register: int;    (* single register. also used for timeout *)
  buffer: bytes;
  mutable read_ptr: int;
  mutable stack: int list;
  animation_registers: int Array.t;
  animations: Pani_anim.t Array.t;
  pics: (int, Bigarray.int8_unsigned_elt) Ndarray.t option Array.t;
  pic_bgnd: (int, Bigarray.int8_unsigned_elt) Ndarray.t option;
}

let make buf_str pic_bgnd pics =
{
  is_done=false;
  timeout=false;
  register=0;
  read_ptr=0;
  buffer=buf_str;
  stack=[];
  animation_registers=Array.make 52 0;
  animations=Array.init 51 (fun _ -> Pani_anim.empty ());
  pics;
  pic_bgnd;
}

type op =
  | CreateAnimation
  | DeleteAnimation
  | SetTimeout
  | DebugWrite
  | DisableAnimation
  | PushSetRegister
    (* Push to stack, either from register or from animation registers *)
  | SetRegisters 
    (* Set main register from code. If 0<value<=50, also set animation reg from stack *)
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
  | SetDone
  | CallFunc
  | Return
  | Error
  [@@deriving show]

let op_of_byte = function
  | 0 -> CreateAnimation
  | 1 -> DeleteAnimation
  | 2 -> SetTimeout
  | 3 -> DebugWrite
  | 4 -> DisableAnimation
  | 5 -> PushSetRegister
  | 6 -> SetRegisters
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
  | 20 -> Error
  | 21 -> SetDone
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
  if debug then
    Printf.printf "0x%x: %s(0x%x)\n" v.read_ptr (show_op op) byte;

  let ret =
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
        | pic_far::delay::reset_y::reset_x::other_anim_idx::anim_idx::data_ptr::rest -> 
          let anim_idx =
            if anim_idx = -1 then (
              Printf.printf "Create animation -1: find unused anim\n";
              begin match Array.find_idx (fun anim -> not anim.Pani_anim.used) v.animations with
              | Some(i,_) -> i
              | None -> 50 
              end
            ) else anim_idx
          in
          if anim_idx > 0 && anim_idx <= 50 then begin
            let anim = 
              let pic_far = pic_far = 1 in
              let buffer = v.buffer in
              Pani_anim.make ~pic_far ~delay ~reset_x ~reset_y ~other_anim_idx ~data_ptr ~buffer
            in
            if debug then
              Printf.printf "Animation %d:\n%s\n" anim_idx (Pani_anim.show anim);
            v.animations.(anim_idx) <- anim
          end;
          v.stack <- rest
        | _ -> print_endline "Invalid stack for animation creation"
        end;
        true
    | DeleteAnimation ->
        begin match v.stack with
        | anim_idx::rest ->
            if anim_idx > 0 && anim_idx <= 50 then begin
              v.animations.(anim_idx).used <- false
            end;
            v.stack <- rest
        | _ -> print_endline "DeleteAnimation: missing anim_idx on stack"
        end;
        true
    | SetTimeout ->
        begin match v.stack with
        | timeout :: rest ->
            if debug then
              Printf.printf "setting timeout of %d\n" timeout;
            v.timeout <- true;
            v.register <- timeout;
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
    | DisableAnimation ->
        begin match v.stack with
        | anim_idx::rest ->
            if anim_idx > 0 && anim_idx <= 50 then begin
              v.animations.(anim_idx).disabled <- true
            end;
            v.stack <- rest
        | _ -> failwith "DisableAnimation: missing argument"
        end;
        true
    | PushSetRegister ->
        let test = read_byte v in
        let value = read_word v in
        if is_true test then begin
          if debug then
            Printf.printf "Push %d from animation_registers[%d]\n" v.animation_registers.(value) value;
          v.register <- v.animation_registers.(value)
        end else begin
          if debug then
            Printf.printf "Push %d\n" value;
          v.register <- value
        end;
        v.stack <- v.register::v.stack;
        true
    | SetRegisters ->
        begin match v.stack with
        | newval::rest ->
          let value = read_word v in
          v.register <- value;
          if value > 0 && value <= 50 then begin
            v.animation_registers.(value) <- newval
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
    | SetDone ->
        v.is_done <- true;
        true
    | Error ->
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
  in
  if debug then
    Printf.printf "reg: %d stack: %s\n" (v.register) (str_of_stack v);
  ret

let step_all_animations v =
  if debug then
    print_endline "\n--- Step through all animations ---\n";

  Array.iteri (fun i anim ->
    Pani_anim.interpret_step anim i;
  )
  v.animations

let enable_all_animations v =
  if debug then
    print_endline "\n--- Enable all animations ---\n";

  let open Pani_anim in
  Array.iter (fun anim ->
    if anim.used then
      anim.disabled <- false;
  )
  v.animations

let step v =
  let rec loop () =
    if v.is_done then `Done else

    if v.timeout then (
      v.register <- v.register - 1;

      if v.register = 0 then (
        v.timeout <- false;
        loop ()
      ) else
        `Timeout
    ) 
    else
      (* Do all processing steps *)
      if interpret v then loop ()
      else `Error
  in
  match loop () with
  | `Timeout ->
      step_all_animations v;
      step_all_animations v;
      step_all_animations v;
      enable_all_animations v;
      `Timeout
  | x -> x

(* Entry point *)
let run_to_end v =
  let rec loop () =
    match step v with
    | `Timeout -> loop ()
    | `Done  -> print_endline "PANI done"
    | `Error -> print_endline "PANI error"
  in
  loop ()

let calc_anim_xy v anim_idx =
  let anim = v.animations.(anim_idx) in
  let open Pani_anim in
  match anim.other_anim_idx with
  | -2 -> anim.x, anim.y
  | other -> 
      (* Assume other_anim_idx is valid or we can't use it *)
      let anim2 = v.animations.(other + 1) in
      (anim2.x + anim.x + anim.reset_x, anim2.y + anim.y + anim.reset_y)

let anim_get_pic v anim_idx =
  let open Pani_anim in
  let anim = v.animations.(anim_idx) in
  match anim.used, anim.disabled, anim.pic_idx with
  | _, _, -1 -> None
  | true, false, i -> Some i
  | _ -> None

