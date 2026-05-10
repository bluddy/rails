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
    |> String.drop 1 |> Option.some
  with Not_found ->
    None

