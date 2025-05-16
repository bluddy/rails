(* Module for stockmarket *)
open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants

let src = Logs.Src.create "stock_market" ~doc:"Stock_market"
module Log = (val Logs.src_log src: Logs.LOG)

module IntMap = Utils.IntMap

type price_history = int list (* reverse history of prices *)
  [@@deriving yojson]

type t = {
  (* Who owns how many shares in what company. By default, all shares are public
     First level: owner. Second level: owned
     *)
  ownership: (int IntMap.t) IntMap.t;
  prices: int IntMap.t; (* share prices *)
  totals: int IntMap.t; (* total number of shares *)
  price_histories: price_history IntMap.t; (* prices over time in fiscal periods *)
} [@@deriving yojson]

let default = {
  ownership=IntMap.empty;
  prices=IntMap.empty;
  totals=IntMap.empty;
  price_histories=IntMap.empty;
}

let player_starting_share_price difficulty = 
    B_options.difficulty_to_enum difficulty + 7

let add_human_player ~player difficulty v =
  let totals = IntMap.add player C.Stock.starting_num v.totals in
  let prices = IntMap.add player (B_options.difficulty_to_enum difficulty + 7) v.prices in
  let price_histories = IntMap.add player [] v.price_histories in
  {v with totals; prices; price_histories}

let add_ai_player ~player ~num_fin_periods v =
  (* AI players come in late, so we need to complete their history *)
  let prices = IntMap.add player C.Stock.ai_share_price v.prices in
  let totals = IntMap.add player C.Stock.starting_num v.prices in
  let history = if num_fin_periods > 0 then List.replicate 0 (num_fin_periods - 1) else [] in
  let price_histories = IntMap.add player history v.price_histories in
  {v with totals; prices; price_histories}

let owned_shares ~owner ~owned v =
  match IntMap.get owner v.ownership with
  | Some owned_map -> 
    IntMap.get_or ~default:0 owned owned_map
  | None -> 0

let self_owned_shares player v = owned_shares ~owner:player ~owned:player v

let treasury_shares player v = owned_shares ~owner:player ~owned:player v

let total_shares player v = IntMap.get_or ~default:0 player v.totals

let non_treasury_shares player v = total_shares player v - treasury_shares player v

let share_price player v = IntMap.get_or ~default:0 player v.prices

let owned_share_value ~total_shares ~owned_shares ~share_price =
  (* We need to account for the cost of selling all our stock 10k shares at a time *)
  let non_treasury_shares = total_shares - owned_shares in
  let sale_rounds = owned_shares / 10 in
  let rec loop i ~total ~share_price =
    if i < sale_rounds then
      let loss = (10 * share_price ) / (10 * i + non_treasury_shares + 10) in
      let share_price = share_price - loss in
      let total = total + share_price in
      loop (i + 1) ~total ~share_price
    else
      total
  in
  loop 0 ~total:0 ~share_price

let treasury_share_value player v =
  (* How much the player owns in himself value-wise *)
  let total_shares = total_shares player v in
  let treasury_shares = treasury_shares player v in
  let share_price = share_price player v in
  owned_share_value ~total_shares ~owned_shares:treasury_shares ~share_price

let add_shares ~owner ~owned ~num v =
  let ownership =
    IntMap.update owner (function
      | Some owneds -> IntMap.incr owned num owneds |> Option.some
      | None -> IntMap.empty |> IntMap.incr owned num |> Option.some)
    v.ownership
  in
  {v with ownership}

let remove_shares ~owner ~owned ~num v = add_shares ~owner ~owned ~num:(-num) v

let set_owned_shares ~owner ~owned num v =
  let ownership =
    IntMap.update owner (function
      | Some owneds -> IntMap.add owned num owneds |> Option.some
      | None -> IntMap.empty |> IntMap.add owned num |> Option.some)
    v.ownership
  in
  {v with ownership}

let reset_owned_shares player v =
  let ownership = IntMap.remove player v.ownership in
  {v with ownership}

let set_total_shares ~player total_shares v =
  let totals = IntMap.add player total_shares v.totals in
  {v with totals}

let add_to_share_price ~player change v =
  let prices = IntMap.incr player change v.prices in
  {v with prices}

let set_share_price ~player ~price v =
  let prices = IntMap.add player price v.prices in
  {v with prices}

let num_other_companies_owner_has_shares_in owner v =
  match IntMap.get owner v.ownership with
  | None -> 0
  | Some owneds ->
    IntMap.sum (fun player shares ->
      if owner <> player && shares > 0 then 1 else 0)
      owneds

let shares_owned_by_all_companies player ?exclude v =
  IntMap.sum (fun owner owneds ->
     match exclude with
     | Some exclude when owner = exclude -> 0
     | _ ->
        IntMap.sum (fun owned shares -> if owned = player then shares else 0) owneds
  ) v.ownership

let shares_owned_by_other_companies player v =
  shares_owned_by_all_companies player ~exclude:player v

  (* Publicly available (unowned) shares *)
let public_shares player v =
  let owned_shares = shares_owned_by_all_companies player v in
  total_shares player v - owned_shares

let controls_company player ~target v =
  let owned_shares = owned_shares ~owner:player ~owned:target v in
  let total_shares = total_shares target v in
  owned_shares > total_shares / 2

let controls_own_company player v = controls_company player ~target:player v

let can_buy_stock ~player ~target ~cash ~difficulty v =
  (* TODO: In original code, it's < total_shares - 10. Not sure why *)
  (* Test if we have an 'anti-trust' problem *)
  let max_owned_companies = Utils.clip (B_options.difficulty_to_enum difficulty) ~min:1 ~max:3 in
  let public_shares = public_shares target v in
  let v2 = add_shares ~owner:player ~owned:target ~num:C.num_buy_shares v in
  if num_other_companies_owner_has_shares_in player v2 > max_owned_companies then
    `Anti_trust_violation max_owned_companies
    (* TODO: double check the below buying out *)
  else if player <> target && public_shares = 0 &&
       not (controls_own_company target v)
    then
      let share_price = share_price target v * 2 in
      let shares_to_buy = shares_owned_by_all_companies target ~exclude:player v in
      `Offer_takeover(share_price, shares_to_buy)
  else (* buy public shares *)
    (* TODO: what about buying the rest of *your* own shares? *)
    let share_price = share_price target v in
    let cost = share_price * C.num_buy_shares in
    if cash >= cost && public_shares > 0 then `Ok
    else `Error

let _sell_buy_stock player ~target ~buy v =
  (* add_stock: for AI. Currently unused. TODO *)
  let share_price = share_price target v in
  let public_shares = public_shares target v in
  let cost = share_price * C.num_buy_shares in
  let price_change =
    let delta = if buy then 1 else 0 in
    (cost / public_shares) + delta
  in
  let price_change = if buy then price_change else -price_change in
  Log.debug (fun f -> f "price change[%d] public_shares[%d] share_price=[%d]" price_change public_shares share_price);
  let v = add_to_share_price ~player:target price_change v in
  let num = if buy then C.num_buy_shares else -C.num_buy_shares in
  let v = add_shares ~owner:player ~owned:target ~num v in
  cost, v

let ai_sell_own_stock ~ai_idx v =
  let v = remove_shares ~owner:ai_idx ~owned:ai_idx ~num:C.num_buy_shares v in
  let ai_treasury_shares = owned_shares ~owner:ai_idx ~owned:ai_idx v in
  let non_treasury_shares = total_shares ai_idx v - ai_treasury_shares + C.num_buy_shares in
  let price_delta = C.num_buy_shares * (share_price ai_idx v) / non_treasury_shares in
  let v = add_to_share_price ~player:ai_idx (-price_delta) v in
  let profit = (share_price ai_idx v) * C.num_buy_shares - 1 in
  profit, v

let buy_stock ~player ~target ~difficulty ~cash v =
  match can_buy_stock ~player ~target ~cash ~difficulty v with
  | `Ok when player = target ->
    (* TODO: add_stock: code adds 10 for ai *)
    let cost, v = _sell_buy_stock player ~target ~buy:true v in
    `Bought cost, v

  | `Offer_takeover(share_price, num_shares) when cash >= share_price * num_shares -> (* buy all *)
    (* Buy all stock for one player. Remove from all others *)
    let money =
      IntMap.mapi (fun owner owned_map ->
        if owner = player then
          (* Cost for buyer *)
          -num_shares * share_price
        else
          let owned_shares = IntMap.get_or ~default:0 target owned_map in
          owned_shares * share_price)
        v.ownership
    in
    let ownership =
      IntMap.mapi (fun owner owned_map ->
        IntMap.mapi (fun owned num ->
          if owned = target then
            if owner = owned then num + num_shares
            else 0 
          else num)
        owned_map)
        v.ownership
    in
    `Takeover money, {v with ownership}

  | _ -> `None, v

let can_sell_stock player ~target v = owned_shares ~owner:player ~owned:target v > 0

let sell_stock player ~target v =
  if can_sell_stock player ~target v then
    _sell_buy_stock player ~target ~buy:false v
  else
    0, v

let other_companies_controlled_by player v =
  match IntMap.get player v.ownership with
  | None -> []
  | Some owned_map ->
    let companies = IntMap.keys owned_map in
    Iter.filter (fun company -> company <> player && controls_company player ~target:company v) companies
    |> Iter.to_list

let controls_any_other_company player v =
  not @@ List.is_empty @@ other_companies_controlled_by player v

  (* Compute value of all stocks a player has in all companies, including itself *)
  (* TODO: double check we're accounting for deprecetiating stock prices *)
let total_owned_stock_value player ?(exclude_self=false) v =
  match IntMap.get player v.ownership with
  | None -> 0
  | Some owned -> 
    IntMap.sum (fun owned num ->
      if exclude_self && owned = player then 0 else
      let share_price = share_price owned v in
      let total_shares = total_shares owned v in
      owned_share_value ~total_shares ~owned_shares:num ~share_price)
      owned

let other_companies_in_player_shares player ?exclude_owner v =
  IntMap.sum (fun owner owned ->
    match exclude_owner with
    | Some exclude_owner when owner = exclude_owner -> 0
    | _ -> IntMap.get_or player owned ~default:0
  )
  v.ownership


