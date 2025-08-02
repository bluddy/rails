open Containers

(* Pani Interpreter: interprets the language of the PANI file *)
(* Notes: pay attention to signed vs unsigned comparisons *)

module Ndarray = Owl_base_dense_ndarray.Generic
module C = Constants.Pani

let sp = Printf.sprintf
let pp = Printf.printf

let debug = ref false

let set_debug x = debug := x

type ndarray = (int, Bigarray.int8_unsigned_elt) Ndarray.t

(* background pic type *)
type pic = {
  x: int;
  y: int;
  pic_idx: int;
}

type debugger = {
  mutable cur_sprite: [`Interp | `Begin | `Some of int ];
  mutable status: [`Delay | `Pause | `Done]
}

let default_debugger = {
  cur_sprite=`Interp;
  status=`Pause;
}

type t = {
  mutable is_done: bool;
  mutable delay: bool;
  mutable delay_time: int;    (* single delay_time. also used for delay *)
  buffer: bytes;
  mutable read_ptr: int;
  mutable stack: int list;
  memory: int Array.t;
  sprites: Pani_sprite.t Array.t;
  pics: ndarray option Array.t;
  background: ndarray option;
  mutable static_pics: pic list;
  debugger: debugger option;
}

let make ?(debug=false) ?(input=[]) buf_str (background: ndarray option) pics =
  assert (Array.length pics = 251);
  let memory = Array.make 52 0 in
  List.iter (fun (loc, v) -> memory.(loc) <- v) input;
  pp "Buffer length is %d\n" (Bytes.length buf_str);
  {
    is_done=false;
    delay=false;
    delay_time=0;
    read_ptr=0;
    buffer=buf_str;
    stack=[];
    memory=Array.make 52 0;
    sprites=Array.init 51 (fun _ -> Pani_sprite.empty ());
    pics; (* size 251 *)
    background;
    static_pics=[];
    debugger=if debug then Some default_debugger else None;
  }

type op =
  | CreateSprite
  | DeleteSprite
  | SetDelay
  | AudioOutput
  | MakeBackground
  | PushSetRegister
    (* Push to stack, either from delay_time or from animation registers *)
  | SetRegisters
    (* Set main delay_time from code. If 0<value<=50, also set animation reg from stack *)
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
  | Pause
  [@@deriving show {with_path=false}]

let op_of_byte = function
  | 0 -> CreateSprite
  | 1 -> DeleteSprite
  | 2 -> SetDelay
  | 3 -> AudioOutput
  | 4 -> MakeBackground
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
  | 20 -> Pause
  | 21 -> SetDone
  | 22 -> Return
  | 23 -> CallFunc
  | x  -> failwith @@ Printf.sprintf "Unknown op code %d" x

let str_of_stack v =
  "[" ^ (String.concat ", " @@ List.map Int.to_string v.stack) ^ "]"

let calc_anim_xy v anim_idx =
  let anim = v.sprites.(anim_idx) in
  let open Pani_sprite in
  match anim.other_sprite with
  | -2 -> anim.x, anim.y
  | other -> 
      (* Assume other_sprite is valid or we can't use it *)
      let anim2 = v.sprites.(other + 1) in
      (anim2.x + anim.x + anim.reset_x, anim2.y + anim.y + anim.reset_y)

let save_sprite i v =
    (* We only save if there's a background flag.
       Also, we only save on deletion since that's when the sprite disappears *)
    let anim = v.sprites.(i) in
    if anim.background then (
      let x, y = calc_anim_xy v i in
      let pic = {x; y; pic_idx=anim.pic_idx} in
      v.static_pics <- pic::v.static_pics)

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
  if !debug then
    Printf.printf "0x%x: %s(0x%x) " v.read_ptr (show_op op) byte;

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
              if !debug then
                Printf.printf "%d %d = %d " y x result;
              result::z
          | _ -> failwith "Cannot add. Stack has < 2 elements"
        in
        v.stack <- stack';
        `Stay
    | CreateSprite ->
        begin match v.stack with
        | pic_far::delay::reset_y::reset_x::other_sprite::anim_idx::data_ptr::rest -> 
          let anim_idx =
            if anim_idx = -1 then (
              if !debug then
                Printf.printf "-1: find unused anim. ";
              begin match Array.find_idx (fun anim -> not anim.Pani_sprite.active) v.sprites with
              | Some(i,_) -> i
              | None -> 50 
              end
            ) else anim_idx
          in
          if anim_idx >= 0 && anim_idx <= 50 then (
            let anim = 
              let pic_far = pic_far = 1 in
              let buffer = v.buffer in
              Pani_sprite.make ~pic_far ~delay ~reset_x ~reset_y ~other_sprite ~data_ptr ~buffer
            in
            if !debug then
              Printf.printf "sprite[%d]\n%s\n" anim_idx (Pani_sprite.show anim);
            (* If we're replacing a live sprite, see if we should save it to background first *)
            if v.sprites.(anim_idx).active then save_sprite anim_idx v;
            v.sprites.(anim_idx) <- anim
          ) else (
            Printf.printf "Error: CreateSprite encountered bad anim idx %d on stack" anim_idx
          );
          v.stack <- rest

        | _ -> print_endline "Invalid stack for animation creation"
        end;
        `Stay

    | DeleteSprite ->
        begin match v.stack with
        | anim_idx::rest ->
            if anim_idx >= 0 && anim_idx <= 50 then begin
              if !debug then
                Printf.printf "%d " anim_idx;
              v.sprites.(anim_idx).active <- false
            end;
            save_sprite anim_idx v;
            v.stack <- rest
        | _ -> print_endline "DeleteAnimation: missing anim_idx on stack"
        end;
        `Stay

    | SetDelay ->
        begin match v.stack with
        | delay :: rest ->
            if !debug then
              Printf.printf "%d " delay;
            v.delay <- true;
            v.delay_time <- delay;
            v.stack <- rest
        | _ -> failwith "SetDelay: missing delay argument"
        end;
        `Stay
    | AudioOutput ->
        begin match v.stack with
        | x::rest ->
            if !debug then
              Printf.printf "audio: %d\n" x;
            v.stack <- rest
        | _ -> failwith "AudioOutput: missing value argument"
        end;
        `Stay
    | MakeBackground ->
        begin match v.stack with
        | anim_idx::rest ->
            if anim_idx >= 0 && anim_idx <= 50 then (
              if !debug then
                Printf.printf "%d " anim_idx;
              let anim = v.sprites.(anim_idx) in
              anim.background <- true;

              v.stack <- rest;
            )
        | _ -> failwith "MakeBackground: missing argument"
        end;
        `Stay
    | PushSetRegister ->
        let test = read_byte v in
        let value = read_word v in
        if is_true test then begin
          if !debug then
            Printf.printf "%d from memory [%d] " v.memory.(value) value;
          v.delay_time <- v.memory.(value)
        end else begin
          if !debug then
            Printf.printf "%d " value;
          v.delay_time <- value
        end;
        v.stack <- v.delay_time::v.stack;
        `Stay
    | SetRegisters ->
        begin match v.stack with
        | newval::rest ->
          let value = read_word v in
          if !debug then
            Printf.printf "reg = %d " value;
          v.delay_time <- value;
          if value >= 0 && value <= 50 then begin
            if !debug then
              Printf.printf ", %d in animation_reg[%d] " newval value;
            v.memory.(value) <- newval
          end;
          v.stack <- rest
        | _ -> failwith "SetDelayWriteAnimArray: missing argument"
        end;
        `Stay
    | Copy ->
        begin match v.stack with
        | x::rest ->
          if !debug then
            Printf.printf "%d " x;
          v.stack <- x::x::rest
        | _ -> failwith "Copy: missing argument"
        end;
        `Stay
    | JumpIfTrue ->
        begin match v.stack with
        | do_jump::rest ->
            let addr = read_word v in
            if is_true do_jump then (
              if !debug then
                Printf.printf "true, jump to 0x%x " addr;
              v.read_ptr <- addr
            ) else (
              if !debug then
                Printf.printf "no jump to 0x%x " addr;
            );
            v.stack <- rest
        | _ -> failwith "JumpIfTrue: missing argument"
        end;
        `Stay
    | Jump ->
        let addr = read_word v in
        if !debug then
          Printf.printf "to 0x%x " addr;
        v.read_ptr <- addr;
        `Stay
    | SetDone ->
        v.is_done <- true;
        `Stay
    | Pause ->
        `Pause
    | CallFunc ->
        let jump_addr = read_word v in
        v.stack <- v.read_ptr :: v.stack;
        v.read_ptr <- jump_addr;
        if !debug then
          Printf.printf "addr 0x%x " jump_addr;
        `Stay
    | Return ->
        begin match v.stack with
        | ret_addr::rest ->
          v.read_ptr <- ret_addr;
          v.stack <- rest;
          if !debug then
            Printf.printf "to 0x%x "ret_addr;
        | _ -> failwith "Return: missing return address"
        end;
        `Stay
  in
  if !debug then
    Printf.printf "\t\treg: %d stack: %s\n" (v.delay_time) (str_of_stack v);
  ret

let step_all_sprites v =
  if !Pani_sprite.debug then
    print_endline "\n--- Step through all animations ---\n";
  Array.iteri (fun i sprite ->
    match Pani_sprite.interpret_step sprite i with
    | `Destroy -> save_sprite i v
    | `None -> ()
  )
  v.sprites

let clear_sprite_background_flags v =
  (* We reuse sprites and don't want them all to be background *)
  let open Pani_sprite in
  Array.iter (fun sprite -> if sprite.active then sprite.background <- false) v.sprites

let step v =
  let rec loop () =
    if v.is_done then `Done else
    if v.delay then (
      let delay_time = v.delay_time in
      v.delay_time <- v.delay_time - 1;

      if delay_time = 0 then (
        v.delay <- false;
        loop ()
      ) else `Delay) 
    else
      (* Do all processing steps *)
      match interpret v with
      | `Pause -> `Pause
      | `Stay -> loop ()
  in
  (* clear_sprite_background_flags v; *)
  match loop () with
  | `Delay | `Pause ->
      step_all_sprites v;
      `Pause
  | `Done -> `Done

let get_debugger v = Option.get_exn_or "Missing debugger" v.debugger 

let find_idx start v =
  let rec loop x =
    if x >= C.max_num_sprites then `Interp
    else if v.sprites.(x).active then `Some x
    else loop (x + 1)
  in
  loop start

let debugger_step_sprite v =
  let d = get_debugger v in 
  begin match d.cur_sprite with
  | `Interp -> ()
  | `Some cur_sprite ->
      d.cur_sprite <- find_idx (cur_sprite + 1) v
  | `Begin ->
      d.cur_sprite <- find_idx 0 v
  end;
  match d.cur_sprite with 
  | `Interp ->
      print_endline "Need interp step";
      `Interp
  | `Some cur_sprite ->
      let sprite = v.sprites.(cur_sprite) in
      begin match Pani_sprite.interpret_step sprite cur_sprite with
      | `Destroy -> save_sprite cur_sprite v
      | `None -> ()
      end;
      `Some cur_sprite
  | `Begin -> failwith "Shouldn't have seen Begin here"

let debugger_step v =
  let d = get_debugger v in
  begin match d.cur_sprite, d.status with
  | `Interp, _ -> ()
  | _, `Done -> ()
  | (`Begin | `Some _), _ ->
    (* Catch up on sprites *)
    let rec loop () = match debugger_step_sprite v with
      | `Interp -> ()
      | _ -> loop ()
    in
    loop()
  end;
  (* Move to next interp instruction *)
  let rec loop () =
    if v.is_done then (
      print_endline "Interp Done";
      `Done
    )
    else
      if v.delay then (
        let delay_time = v.delay_time in
        v.delay_time <- v.delay_time - 1;

        if delay_time = 0 then (
          v.delay <- false;
          loop ()
        ) else ( 
           print_endline "Interp Delay";
          `Delay
        ))
      else
        (* Do all processing steps *)
        match interpret v with
        | `Pause ->
            print_endline "Interp Pause";
            `Pause
        | `Stay -> loop ()
  in
  (* clear_sprite_background_flags v; *)
  d.status <- loop ();
  d.cur_sprite <- `Begin

(* Entry point *)
let dump_run_to_end v =
  debug := true;
  Pani_sprite.debug := true;
  let rec loop () =
    match step v with
    | `Pause -> loop ()
    | `Done  -> print_endline "PANI done"
  in
  loop ()

let anim_get_pic v anim_idx =
  v.sprites.(anim_idx)


