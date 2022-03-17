open Containers

(* Pani Interpreter: interprets the language of the PANI file *)
(* Notes: pay attention to signed vs unsigned comparisons *)

let debug = true

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

let make ~data_ptr ~other_anim_idx ~x_diff ~y_diff ~delay ~pic_far =
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

