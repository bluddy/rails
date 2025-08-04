open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type mode =
  | EnterName of Text_entry.t
  | Display

type entry = {
  player: string;
  rr_name: string;
  job: Jobs.t;
  bonus: Money.t;
  difficulty: B_options.difficulty;
  region: Region.t;
  year_start: int;
  year: int;
} [@@deriving yojson]

type entries = entry list
  [@@deriving yojson]

type t = {
  mode: mode;
  entries: entries;
  idx: int option;
}
