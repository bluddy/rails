open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

module C = Constants

type t = {
  time_months: int;
  caught_mms: Org.Global_set.t;
  gender: Gender.t;
  codename: string;
  difficulty: Difficulty.t;
} [@@deriving yojson]

(* First time *)
let default (info: Start_menu.info) = {
  time_months=0;
  caught_mms=Org.Global_set.empty;
  gender=info.gender;
  codename=info.name;
  difficulty=info.difficulty;
}

