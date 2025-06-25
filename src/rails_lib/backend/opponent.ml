open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open Containers

type name =
  | CorneliusVanderbilt
  | DanielDrew
  | JimFisk
  | JayGould
  | ErastusCorning
  | JPierpontMorgan
  | JEdgarThompson
  | JimHill
  | JayCooke
  | JohnForbes
  | CzarNicholasII
  | VILenin
  | CharlesDeGaulle
  | NapoleonIII
  | OttoVonBismarck
  | BenitoMussolini
  | GeorgeStephenson
  | RobertStephenson
  | IsambardKBrunel
  | GeorgeSHudson
  | HelmuthVonMoltke
  | BaronRothschild
  [@@deriving yojson, ord, eq]

let show_name = function
  | CorneliusVanderbilt -> "Cornelius Vanderbilt"
  | DanielDrew -> "Daniel Drew"
  | JimFisk -> "Jim Fisk"
  | JayGould -> "Jay Gould"
  | ErastusCorning -> "Erastus Corning"
  | JPierpontMorgan -> "J.Pierpont Morgan"
  | JEdgarThompson -> "J.Edgar Thompson"
  | JimHill -> "Jim Hill"
  | JayCooke -> "Jay Cooke"
  | JohnForbes -> "John Forbes"
  | CzarNicholasII -> "Czar Nicholas II"
  | VILenin -> "V.I. Lenin"
  | CharlesDeGaulle -> "Charles De Gaulle"
  | NapoleonIII -> "Napolean III"
  | OttoVonBismarck -> "Otto von Bismarck"
  | BenitoMussolini -> "Benito Mussolini"
  | GeorgeStephenson -> "George Stephenson"
  | RobertStephenson -> "Robert Stephenson"
  | IsambardKBrunel -> "Isambard K.Brunel"
  | GeorgeSHudson -> "George S.Hudson"
  | HelmuthVonMoltke -> "Helmuth von Moltke"
  | BaronRothschild -> "Baron Rothschild"

type t = {
  name: name;
  expansionist: int;  (* TODO: might have to switch with build_skill *)
  financial_skill: int;
  management: int;  (* Skill at operating railroads *)
  build_skill: int;
} [@@deriving yojson]

module Map = Utils.Map.Make(struct
  type nonrec t = name
  let compare = compare_name
  let yojson_of_t = yojson_of_name
  let t_of_yojson = name_of_yojson
end)


let map_of_leaders leaders =
  let make name expansionist financial_skill management build_skill =
    {name; expansionist; financial_skill; management; build_skill}
  in
  leaders
  |> List.map (fun (a, (b, c, d, e)) -> a, make a b c d e)
  |> Map.of_list

let us_leaders = [
  CorneliusVanderbilt, (3, 3, 1, 1);
  DanielDrew, (1, 3, 1, 2);
  JimFisk, (0, 3, 0, 2);
  JayGould, (0, 2, 0, 4);
  ErastusCorning, (4, 1, 3, 0);
  JPierpontMorgan, (2, 4, 2, 1);
  JEdgarThompson, (2, 2, 4, 2);
  JimHill, (4, 1, 2, 2);
  JayCooke, (2, 4, 1, 0);
  JohnForbes, (4, 2, 3, 1);
]

let eu_leaders = [
  CzarNicholasII, (3, 2, 0, 1);
  VILenin, (1, 2, 3, 4);
  CharlesDeGaulle, (1, 2, 2, 3);
  NapoleonIII, (3, 1, 0, 1);
  OttoVonBismarck, (3, 1, 3, 3);
  BenitoMussolini, (1, 1, 4, 3);
  GeorgeStephenson, (4, 1, 3, 1);
  RobertStephenson, (3, 2, 4, 0);
  IsambardKBrunel, (4, 0, 4, 1);
  GeorgeSHudson, (1, 4, 1, 4);
  HelmuthVonMoltke, (3, 0, 4, 3);
  BaronRothschild, (2, 4, 1, 3);
]

let leader_map = us_leaders @ eu_leaders |> map_of_leaders

let random_of_region region random =
  let leaders = if Region.is_us region then us_leaders else eu_leaders in
  Random.pick_list leaders random |> fst

let t_of_leader leader = Map.find leader leader_map

let get_name v = v.name

let show v = show_name v.name

