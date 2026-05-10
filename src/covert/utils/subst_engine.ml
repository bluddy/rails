open Containers
open Re

let get_lines ~pat =
  let pat_r = compile (seq [
    str (pat);
    group (rep (compl [set "*"]))
  ]) in
  fun text ->
  try
    let subs = exec pat_r text in
    Group.get subs 1
  with Not_found ->
    failwith @@ Printf.sprintf "Couldn't find pattern %s in text" pat

