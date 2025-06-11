open Containers
module C = Constants

type t =
  | Mail       (* 0 *)
  | Passengers (* 3 *)
  | Food       (* 6 *)
  | Livestock  (* 7 *)
  | MfgGoods   (* 8 *)
  | Fertilizer (* 9 *)
  | Textiles   (* 10 *)
  | Steel      (* 11 *)
  | Petroleum  (* 12 *)
  | Cotton     (* 13 *)
  | Coal       (* 14 *)
  | Grain
  | Paper
  | Wood
  | Armaments
  | Nitrates
  | Beer
  | Hops
  | Chemicals
  | Wool (* EU *)
  | Grapes (* EU *)
  | Wine (* EU *)
  [@@deriving yojson, ord, enum, show, eq]

let order = List.((to_enum Mail) -- (to_enum Wine))
  |> List.map of_enum |> List.map (Option.get_exn_or "error")

let show = function
  | Mail -> "Mail"
  | Passengers -> "Passengers"
  | Food -> "Food"
  | Livestock -> "Livestock"
  | MfgGoods -> "Mfg. Goods"
  | Fertilizer -> "Fertilizer"
  | Textiles -> "Textiles"
  | Steel -> "Steel"
  | Petroleum -> "Petroleum"
  | Cotton -> "Cotton"
  | Coal -> "Coal"
  | Grain -> "Grain"
  | Paper -> "Paper"
  | Wood -> "Wood"
  | Armaments -> "Armaments"
  | Nitrates -> "Nitrates" 
  | Beer -> "Beer"
  | Hops -> "Hops"
  | Chemicals -> "Chemicals"
  | Wool -> "Wool"
  | Grapes -> "Grapes"
  | Wine -> "Wine"

let car_str_of x =
  let s = match x with
    | Wood -> " Hopper"
    | _ -> " Car"
  in
  (show x)^s

let group_of = function
  | Mail -> "sacks of"
  | Passengers -> ""
  | _ -> "tons of"

let descr_of good amount =
  Printf.sprintf "%d %s %s" (amount / C.goods_div) (group_of good) (show good)

let short_descr_of good amount =
  let short_group = match good with
    | Passengers -> ""
    | _ -> "tn"
  in
  Printf.sprintf "%d %s %s" (amount / C.goods_div) short_group (show good)

let us = [
  Mail; Passengers;
  Food; Livestock; MfgGoods;
  Grain; Paper; Steel;
  Petroleum; Wood; Coal
]

let england = [
  Mail; Passengers;
  Beer; Livestock; MfgGoods;
  Hops; Textiles; Steel;
  Chemicals; Cotton; Coal;
]

let europe = [
  Mail; Passengers;
  Wine; Grapes; Armaments;
  Fertilizer; Textiles; Steel;
  Nitrates; Wool; Coal;
]

let of_region = function
  | Region.WestUS | EastUS -> us
  | Europe -> europe
  | Britain -> england

  (* Conversion tables:
    Each region can only convert one good to one other good.
    ENHANCE: we want to expand this a lot
    *)
let us_convert = [
    Livestock, Food;
    Fertilizer, Food;
    Steel, MfgGoods;
    Petroleum, MfgGoods;
    Cotton, Textiles;
    Coal, Steel;
] |> Hashtbl.of_list

let eu_convert =
  [
    Grapes, Wine;
    Steel, Armaments;
    Wool, Textiles;
    Nitrates, Fertilizer;
    Coal, Steel;
  ] |> Hashtbl.of_list

let en_convert =
  [
    Hops, Beer;
    Steel, MfgGoods;
    Chemicals, MfgGoods;
    Cotton, Textiles;
    Coal, Steel;
  ] |> Hashtbl.of_list

let convert region goods =
  let tbl =
    match region with
    | Region.WestUS | EastUS -> us_convert
    | Europe -> eu_convert
    | Britain -> en_convert
  in
  Hashtbl.find_opt tbl goods

module Set = struct
  (* Use a regular set because we don't know how many goods we'll have *)
  include Utils.Set.Make(struct
    type nonrec t = t 
    let compare = compare
    let t_of_yojson = t_of_yojson
    let yojson_of_t = yojson_of_t
  end) [@@deriving yojson]
end

module Map = struct
  include Utils.Map.Make(struct
    type nonrec t = t
    let compare = compare
    let t_of_yojson = t_of_yojson
    let yojson_of_t = yojson_of_t
  end) [@@ deriving yojson]
end


