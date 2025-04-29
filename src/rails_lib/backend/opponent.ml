open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
  [@@deriving yojson, ord]

type t = {
  valuation: int;
  loan_tolerance: int;
  moneymaking: int;
  dist_div: int;
} [@@deriving yojson]

module Map = Utils.Map.Make(struct
  type nonrec t = name
  let compare = compare_name
  let yojson_of_t = yojson_of_name
  let t_of_yojson = name_of_yojson
end)


let map_of_leaders leaders =
  let make valuation loan_tolerance moneymaking dist_div =
    {valuation; loan_tolerance; moneymaking; dist_div}
  in
  leaders
  |> List.map (fun (a, b, c, d, e) -> a, make b c d e)
  |> Map.of_list

let us_leaders = [
  CorneliusVanderbilt, 3, 3, 1, 1;
  DanielDrew, 1, 3, 1, 2;
  JimFisk, 0, 3, 0, 2;
  JayGould, 0, 2, 0, 4;
  ErastusCorning, 4, 1, 3, 0;
  JPierpontMorgan, 2, 4, 2, 1;
  JEdgarThompson, 2, 2, 4, 2;
  JimHill, 4, 1, 2, 2;
  JayCooke, 2, 4, 1, 0;
  JohnForbes, 4, 2, 3, 1;
] |> map_of_leaders

let eu_leaders = [
  CzarNicholasII, 3, 2, 0, 1;
  VILenin, 1, 2, 3, 4;
  CharlesDeGaulle, 1, 2, 2, 3;
  NapoleonIII, 3, 1, 0, 1;
  OttoVonBismarck, 3, 1, 3, 3;
  BenitoMussolini, 1, 1, 4, 3;
  GeorgeStephenson, 4, 1, 3, 1;
  RobertStephenson, 3, 2, 4, 0;
  IsambardKBrunel, 4, 0, 4, 1;
  GeorgeSHudson, 1, 4, 1, 4;
  HelmuthVonMoltke, 3, 0, 4, 3;
  BaronRothschild, 2, 4, 1, 3;
] |> map_of_leaders

let leaders = Map.union (fun _ _ x -> Some x) us_leaders eu_leaders

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

