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

let time_date_print ~minutes ~months =
  let mins_in_hour = minutes mod 60 in
  let hours = minutes / 60 in
  let hours_in_day = hours mod 24 in
  let hours_am_pm = ((hours_in_day + 11) mod 12) + 1 in
  let am_pm = if hours_in_day <= 11 then 'A' else 'P' in
  let month_of_year = months mod 12 |> str_of_month in
  let day_of_month = hours / 24 + 1 in
  Printf.sprintf "%02d:%02d %cM %s %02d"
    hours_am_pm mins_in_hour am_pm month_of_year day_of_month

