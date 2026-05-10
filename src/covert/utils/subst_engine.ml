open Containers
open Re

let get_lines ~pat =
  let pat_r = compile (seq [
    seq [bol; str pat; eol];
    rep (seq [bol; char '*'; rep any; eol]);
    group (rep (compl [set "*"]))
  ]) in
  fun text ->
  try
    let subs = exec pat_r text in
    Group.get subs 1
    |> String.map (function '\n' -> ' ' | x -> x)
    |> String.drop 1 |> Option.some
  with Not_found ->
    None

