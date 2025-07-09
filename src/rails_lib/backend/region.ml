
type t =
  | EastUS
  | WestUS
  | Britain
  | Europe
  [@@deriving enum, eq, yojson]

let regions = [EastUS; WestUS; Britain; Europe]

let show = function
  | EastUS -> "Eastern USA"
  | WestUS -> "Western USA"
  | Britain -> "England"
  | Europe -> "Europe"

  (* TODO: fix years *)
let start_year = function
  | EastUS -> 1830
  | WestUS -> 1866
  | Britain -> 1828
  | Europe -> 1900

let money_symbol = function
  | EastUS | WestUS -> '$'
  | Britain | Europe -> '#' 

let is_us = function
  | EastUS
  | WestUS -> true
  | Britain
  | Europe -> false

let is_east_us = function
  | EastUS -> true
  | _ -> false

let is_west_us = function
  | WestUS -> true
  | _ -> false

let is_europe = function
  | Europe -> true
  | _ -> false

  (* Handle larger scale of Europe map *)
let dist_mult = function
  | Europe -> 2
  | _ -> 1

  (* Avoid Money for circularity *)
let num_bonds region amount =
  let divide = match region with
    | WestUS -> 1000
    | _ -> 500
  in
  amount / divide

