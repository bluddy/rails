
type t = [ `Male | `Female ]
  [@@deriving yojson]

let of_code code =
  if code land 1 = 1 then `Male else `Female
