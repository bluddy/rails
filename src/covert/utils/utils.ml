open Containers
include Engine.Utils

let add_newlines width str =
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


