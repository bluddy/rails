
type t =
  | EastUS
  | WestUS
  | Britain
  | Europe
  [@@deriving enum, eq]

let regions = [EastUS; WestUS; Britain; Europe]

