open Containers
open Re

let get_lines ~pat =
  let pat = compile (seq [
    str pat;
    group (rep (compl [set "\n*"]))
  ]) in
  fun text ->
  try
    let subs = exec pat text in
    Group.get subs 1 |> Option.some
  with Not_found -> None

