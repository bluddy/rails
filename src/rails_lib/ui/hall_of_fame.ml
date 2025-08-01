
type mode =
  | EnterName of Text_entry.t
  | Display of {idx: int; text: string}

type t = {
  mode: mode;
}
