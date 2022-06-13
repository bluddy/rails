open Containers

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
  | Caboose
  [@@deriving sexp]

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
  | Caboose -> "Caboose"

type freight =
  | FreightMail
  | FreightPassenger
  | FreightFast
  | FreightSlow
  | FreightBulk

let freight_of_goods = function
  | Caboose
  | Mail -> FreightMail
  | Passengers -> FreightPassenger
  | Food
  | Livestock
  | Beer
  | Wine
  | Grapes
  | Armaments
  | MfgGoods -> FreightFast
  | Fertilizer
  | Textiles
  | Steel
  | Grain
  | Paper
  | Hops -> FreightSlow
  | Cotton
  | Coal
  | Chemicals
  | Petroleum
  | Wood
  | Nitrates
  | Wool -> FreightBulk


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


