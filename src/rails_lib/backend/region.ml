
type t =
  | EastUS
  | WestUS
  | Britain
  | Europe
  [@@deriving enum, eq, yojson]

let regions = [EastUS; WestUS; Britain; Europe]

let money_symbol = function
  | EastUS | WestUS -> '$'
  | Britain | Europe -> '#' 

let is_us = function
  | EastUS
  | WestUS -> true
  | Britain
  | Europe -> false

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
