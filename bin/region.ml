
type t =
  | EastUS
  | WestUS
  | Britain
  | Europe
  [@@deriving enum, eq]

let regions = [EastUS; WestUS; Britain; Europe]

let money_symbol = function
  | EastUS | WestUS -> "$"
  | Britain | Europe -> "#" 

let is_us = function
  | EastUS
  | WestUS -> true
  | Britain
  | Europe -> false
