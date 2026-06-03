open Containers
include Engine.Utils

let add_newlines ~width str =
  let buf = Buffer.create 10 in
  let _ =
    String.fold_left (fun x c ->
      let x = x + 1 in
      match c with
      | ' ' when x > width ->
        Buffer.add_char buf '\n';
        0
      | _ ->
        Buffer.add_char buf c;
        x)
    0
    str
  in
  Buffer.contents buf

let try_do ~init check_fail =
  let rec loop n =
    let x = init () in
    if n >= 999 then x else
    if check_fail x then loop (n + 1)
    else x
  in
  loop 0

let do_while do_fn while_fn =
  let rec loop () =
    let x = do_fn () in
    if while_fn x then loop ()
    else x
  in
  loop ()

let do_until do_fn until_fn =
  do_while do_fn (fun x -> not @@ until_fn x)

