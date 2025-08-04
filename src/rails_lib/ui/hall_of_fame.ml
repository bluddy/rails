open Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers
module C = Constants.HallOfFame

type mode =
  | EnterName of Text_entry.t
  | Display of {selected: int option}

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
  entries: entry list;
}

let hof_file = "./hof.dat"

let make ?bonus () =
  let v =
    if IO.File.exists hof_file then
      let s = IO.File.read_exn hof_file in
      let entries = Yojson.Safe.from_string s |> entries_of_yojson in
      let v = match List.find_idx (fun entry -> bonus > entry.bonus) entries with
        | Some (_, i) ->
        | None when List.length entries < C.max_entries -> 
        | None -> entries
    else
      {mode=Display{selected=None}; entries=[]}
  in




let make = ()
