(* Module for stockmarket *)
open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants
module M = Money

let src = Logs.Src.create "stock_market" ~doc:"Stock_market"
module Log = (val Logs.src_log src: Logs.LOG)

type price_history = Money.t list (* reverse history of prices *)
  [@@deriving yojson]

type t = {
  (* Who owns how many shares in what company. By default, all shares are public
     First level: owner. Second level: owned
     *)
  ownership: (int Owner.Map.t) Owner.Map.t;
  prices: M.t Owner.Map.t; (* share prices *)
  totals: int Owner.Map.t; (* total number of shares *)
  value_histories: price_history Owner.Map.t; (* prices over time in fiscal periods *)
  avg_prices: M.t Owner.Map.t;
} [@@deriving yojson]

let default = {
  ownership=Owner.Map.empty;
  prices=Owner.Map.empty;
  totals=Owner.Map.empty;
  value_histories=Owner.Map.empty;
  avg_prices=Owner.Map.empty;
}

let player_starting_share_price difficulty = 
    B_options.difficulty_to_enum difficulty + 7

let add_human_player player_idx difficulty v =
  let totals = Owner.Map.add player_idx C.Stock.starting_num v.totals in
  let prices = Owner.Map.add player_idx (M.of_int @@ B_options.difficulty_to_enum difficulty + 7) v.prices in
  let value_histories = Owner.Map.add player_idx [] v.value_histories in
  {v with totals; prices; value_histories}

let add_ai_player player_idx ~num_fin_periods v =
  (* AI players come in late, so we need to complete their history *)
  let prices = Owner.Map.add player_idx C.Stock.ai_share_price v.prices in
  let totals = Owner.Map.add player_idx C.Stock.starting_num v.totals in
  let history = if num_fin_periods > 0 then
    List.replicate (num_fin_periods - 1) M.zero 
    else []
  in
  let value_histories = Owner.Map.add player_idx history v.value_histories in
  {v with totals; prices; value_histories}

let owned_shares owner ~owned v =
  match Owner.Map.get owner v.ownership with
  | Some owned_map -> 
    Owner.Map.get_or ~default:0 owned owned_map
  | None -> 0

let self_owned_shares player_idx v = owned_shares player_idx ~owned:player_idx v

let treasury_shares player_idx v = owned_shares player_idx ~owned:player_idx v

let total_shares player_idx v = Owner.Map.get_or ~default:0 player_idx v.totals

let non_treasury_shares player_idx v = total_shares player_idx v - treasury_shares player_idx v

let share_price player_idx v = Owner.Map.get_or ~default:M.zero player_idx v.prices

let set_share_price player_idx price v = Owner.Map.add player_idx price v.prices

let avg_share_price player_idx v = Owner.Map.get_or ~default:M.zero player_idx v.avg_prices

let set_avg_share_price player_idx price v = Owner.Map.add player_idx price v.avg_prices

let owned_share_value ~total_shares ~owned_shares ~(share_price:M.t) =
  (* We need to account for the cost of selling all our stock 10k shares at a time *)
  let non_treasury_shares = total_shares - owned_shares in
  let sale_rounds = owned_shares / 10 in
  let rec loop i ~total ~share_price =
    if i < sale_rounds then
      let loss = M.div M.(share_price * 10) (10 * i + non_treasury_shares + 10) in
      let share_price = M.(share_price - loss) in
      let total = M.(total + share_price) in
      loop (i + 1) ~total ~share_price
    else
      total
  in
  loop 0 ~total:M.zero ~share_price

let treasury_share_value player_idx v =
  (* How much the player owns in himself value-wise *)
  let total_shares = total_shares player_idx v in
  let treasury_shares = treasury_shares player_idx v in
  let share_price = share_price player_idx v in
  owned_share_value ~total_shares ~owned_shares:treasury_shares ~share_price

let add_shares owner ~owned ~num v =
  let ownership =
    Owner.Map.update owner (function
      | Some owneds -> Owner.Map.incr owned num owneds |> Option.some
      | None -> Owner.Map.empty |> Owner.Map.incr owned num |> Option.some)
    v.ownership
  in
  {v with ownership}

let remove_shares owner ~owned ~num v = add_shares owner ~owned ~num:(-num) v

let set_owned_shares owner ~owned num v =
  let ownership =
    Owner.Map.update owner (function
      | Some owneds -> Owner.Map.add owned num owneds |> Option.some
      | None -> Owner.Map.empty |> Owner.Map.add owned num |> Option.some)
    v.ownership
  in
  {v with ownership}

let reset_owned_shares player_idx v =
  let ownership = Owner.Map.remove player_idx v.ownership in
  {v with ownership}

let set_total_shares player_idx total_shares v =
  let totals = Owner.Map.add player_idx total_shares v.totals in
  {v with totals}

let add_to_share_price player_idx change v =
  let prices = Owner.Map.incr_cash player_idx change v.prices in
  {v with prices}

let set_share_price player_idx price v =
  let prices = Owner.Map.add player_idx price v.prices in
  {v with prices}

let set_avg_share_price player_idx price v =
  let prices = Owner.Map.add player_idx price v.avg_prices in
  {v with prices}

let num_other_companies_owner_has_shares_in owner v =
  match Owner.Map.get owner v.ownership with
  | None -> 0
  | Some owneds ->
    Owner.Map.sum (fun player_idx shares ->
      if Owner.(owner <> player_idx) && shares > 0 then 1 else 0)
      owneds

let shares_owned_by_all_companies player_idx ?exclude v =
  Owner.Map.sum (fun owner owneds ->
     match exclude with
     | Some exclude when Owner.(owner = exclude) -> 0
     | _ ->
        Owner.Map.sum (fun owned shares -> if Owner.(owned = player_idx) then shares else 0) owneds
  ) v.ownership

let shares_owned_by_other_companies player_idx v =
  shares_owned_by_all_companies player_idx ~exclude:player_idx v

  (* Publicly available (unowned) shares *)
let public_shares player_idx v =
  let owned_shares = shares_owned_by_all_companies player_idx v in
  total_shares player_idx v - owned_shares

let controls_company player_idx ~target v =
  let owned_shares = owned_shares player_idx ~owned:target v in
  let total_shares = total_shares target v in
  owned_shares > total_shares / 2

let hostile_takeover ~ai_idx ~player_idx v =
  (* End of game for player *)
  v
  |> add_shares ai_idx ~owned:player_idx ~num:(treasury_shares player_idx v)
  |> set_owned_shares player_idx ~owned:ai_idx 0
  |> set_owned_shares player_idx ~owned:player_idx 0

let controls_own_company player_idx v = controls_company player_idx ~target:player_idx v

let can_buy_stock player_idx ~target ~cash (params:Params.t) v =
  (* TODO: In original code, it's < total_shares - 10. Not sure why *)
  (* Test if we have an 'anti-trust' problem *)
  let max_owned_companies = Utils.clip (B_options.difficulty_to_enum params.options.difficulty) ~min:1 ~max:3 in
  let public_shares = public_shares target v in
  let v2 = add_shares player_idx ~owned:target ~num:C.num_buy_shares v in
  if num_other_companies_owner_has_shares_in player_idx v2 > max_owned_companies then
    `Anti_trust_violation max_owned_companies
    (* TODO: double check the below buying out *)
  else if Owner.(player_idx <> target) && public_shares = 0 &&
       not (controls_own_company target v)
    then
      let share_price = M.(share_price target v * 2) in
      let shares_to_buy = shares_owned_by_all_companies target ~exclude:player_idx v in
      `Offer_takeover(share_price, shares_to_buy)
  else (* buy public shares *)
    (* TODO: what about buying the rest of *your* own shares? *)
    let share_price = share_price target v in
    let cost = M.(share_price * C.num_buy_shares) in
    if M.(cash >= cost) && public_shares > 0 then `Ok
    else `Error

    (* TODO: check the logic here for sell. It might be wrong *)
let _sell_buy_stock player_idx ~target ~buy v =
  let share_price = share_price target v in
  let public_shares = public_shares target v in
  let cost = M.(share_price * C.num_buy_shares) in
  let price_change =
    let open M in
    let delta = if buy then 1 else 0 in
    (cost / public_shares) +~ delta
  in
  let price_change = if buy then price_change else M.neg price_change in
  let v = add_to_share_price target price_change v in
  let num = if buy then C.num_buy_shares else -C.num_buy_shares in
  let v = add_shares player_idx ~owned:target ~num v in
  cost, v

  (* TODO: compare logic to player buying/selling.
     1: it should be public shares, not non-treasury-shares
     2: the cost should be determined early, not afterwards
     3: ai selling its own stock has a weird order of operations
   *)
let ai_buy_stock ~ai_idx ~player_idx ~human v =
  let modify = if human then M.(fun x -> x + of_int 1) else Fun.id in
  let cost = M.((share_price player_idx v) * C.num_buy_shares) in
  let player_treasury_shares = treasury_shares player_idx v in
  (* BUG: the original code add num_buy_shares here, probably copied from sell *)
  let non_treasury_shares = total_shares player_idx v - player_treasury_shares in
  let price_change = modify @@ M.(cost / non_treasury_shares) in
  let v = add_to_share_price player_idx price_change v in
  let v = add_shares ai_idx ~owned:player_idx ~num:C.num_buy_shares v in
  let cost = modify @@ M.((share_price player_idx v) * C.num_buy_shares) in
  cost, v

let ai_buy_player_stock ~ai_idx ~player_idx v =
  ai_buy_stock ~ai_idx ~player_idx ~human:true v

let ai_buy_own_stock ~ai_idx v =
  ai_buy_stock ~ai_idx ~player_idx:ai_idx ~human:false v

  (* Weird inconsistency here. Should be fixed *)
let ai_sell_player_stock ~ai_idx ~player_idx v =
  let cost = M.(share_price player_idx v * C.num_buy_shares) in
  (* Doesn't account for public shares *)
  let player_treasury_shares = treasury_shares player_idx v in
  let non_treasury_shares = total_shares player_idx v - player_treasury_shares in
  let price_change = M.(cost / non_treasury_shares) in
  let v = add_to_share_price player_idx (M.neg price_change) v in
  let v = remove_shares ai_idx ~owned:player_idx ~num:C.num_buy_shares v in
  let profit = M.((share_price player_idx v) * C.num_buy_shares -~ 1) in
  profit, v

let ai_sell_own_stock ~ai_idx v =
  let v = remove_shares ai_idx ~owned:ai_idx ~num:C.num_buy_shares v in
  let ai_treasury_shares = treasury_shares ai_idx v in
  let non_treasury_shares = total_shares ai_idx v - ai_treasury_shares + C.num_buy_shares in
  let open M in
  let price_delta = (share_price ai_idx v) * C.num_buy_shares / non_treasury_shares in
  let v = add_to_share_price ai_idx (neg price_delta) v in
  let profit = (share_price ai_idx v) * C.num_buy_shares -~ 1 in
  profit, v

let buy_stock player_idx ~target params ~cash v =
  match can_buy_stock player_idx ~target ~cash params v with
  | `Ok when Owner.(player_idx = target) ->
    (* TODO: add_stock: code adds 10 for ai *)
    let cost, v = _sell_buy_stock player_idx ~target ~buy:true v in
    `Bought cost, v

  | `Offer_takeover(share_price, num_shares) when M.(cash >= share_price * num_shares) -> (* buy all *)
    (* Buy all stock for one player. Remove from all others *)
    let money =
      let open M in
      Owner.Map.mapi (fun owner owned_map ->
        if Owner.(owner = player_idx) then
          (* Cost for buyer *)
          share_price * -num_shares
        else
          let owned_shares = Owner.Map.get_or ~default:0 target owned_map in
          share_price * owned_shares)
        v.ownership
    in
    let ownership =
      Owner.Map.mapi (fun owner owned_map ->
        Owner.Map.mapi (fun owned num ->
          if Owner.(owned = target) then
            if Owner.(owner = owned) then num + num_shares
            else 0 
          else num)
        owned_map)
        v.ownership
    in
    `Takeover money, {v with ownership}

  | _ -> `None, v

let can_sell_stock player_idx ~target v = owned_shares player_idx ~owned:target v > 0

let sell_stock player_idx ~target v =
  if can_sell_stock player_idx ~target v then
    _sell_buy_stock player_idx ~target ~buy:false v
  else
    M.zero, v

let other_companies_controlled_by player_idx v =
  match Owner.Map.get player_idx v.ownership with
  | None -> []
  | Some owned_map ->
    let companies = Owner.Map.keys owned_map in
    Iter.filter (fun company -> Owner.(company <> player_idx) && controls_company player_idx ~target:company v) companies
    |> Iter.to_list

let controls_any_other_company player_idx v =
  not @@ List.is_empty @@ other_companies_controlled_by player_idx v

  (* Compute value of all stocks a player has in all companies, including itself *)
  (* TODO: double check we're accounting for deprecetiating stock prices *)
let total_owned_stock_value player_idx ?(exclude_self=false) v =
  match Owner.Map.get player_idx v.ownership with
  | None -> M.zero
  | Some owned -> 
    Owner.Map.sum_cash (fun owned num ->
      if exclude_self && Owner.(owned = player_idx) then M.zero else
      let share_price = share_price owned v in
      let total_shares = total_shares owned v in
      owned_share_value ~total_shares ~owned_shares:num ~share_price)
      owned

let other_companies_in_player_shares player_idx ?exclude_owner v =
  Owner.Map.sum (fun owner owned ->
    match exclude_owner with
    | Some exclude_owner when Owner.(owner = exclude_owner) -> 0
    | _ -> Owner.Map.get_or player_idx owned ~default:0
  )
  v.ownership

let declare_bankruptcy player_idx players v =
  (* Returns new stock market, cash for each player_idx *)
  let share_price = share_price player_idx v in
  Iter.fold (fun (stocks, cash) idx ->
    if Owner.(idx = player_idx) then
      let stocks = stocks
        |> set_total_shares player_idx C.Stock.starting_num
        |> reset_owned_shares player_idx
      in
      stocks, cash
    else
      (* Other players sell all stock in company and get partially compensated *)
      let sold_stock = M.((share_price * owned_shares idx ~owned:player_idx stocks) / 2) in
      let cash = Owner.Map.add idx sold_stock cash in
      let stocks = set_owned_shares idx ~owned:player_idx 0 stocks in
      stocks, cash
  )
  (v, Owner.Map.empty)
  players

let split_stock player_idx v =
  let prices = Owner.Map.update player_idx (Option.map @@ fun price -> M.(price / 2)) v.prices in
  let avg_prices = Owner.Map.update player_idx (Option.map @@ fun price -> M.(price / 2)) v.avg_prices in
  let totals = Owner.Map.update player_idx (Option.map @@ ( * ) 2) v.totals in
  let ownership = Owner.Map.map (Owner.Map.update player_idx (Option.map @@ ( * ) 2)) v.ownership in
  {v with prices; avg_prices; totals; ownership}

(* Value of all player stock *)
let stock_value player_idx v =
  let total_shares = total_shares player_idx v in
  let share_price = share_price player_idx v in
  M.(share_price * Int.(total_shares / 100))

let update_history player_idx value v =
  let value_histories =
    Owner.Map.update player_idx (function
      | None -> Some [value]
      | Some xs -> Some(value::xs))
      v.value_histories
  in
  {v with value_histories}

let update_history_with_stock_value player_idx v =
  let value = stock_value player_idx v in
  update_history player_idx value v
  
let investor_opinion growth_pct =
  if growth_pct >= 25 then Ui_msg.Investors_ecstatic else
  if growth_pct >= 10 then Investors_pleased else
  if growth_pct > 5 then Investors_concerned else
  if growth_pct > 2 then Investors_very_concerned else
  Investors_outraged

let investor_anger_mod = function
  | Ui_msg.Investors_ecstatic -> -1
  | Investors_pleased -> -1
  | Investors_concerned -> 0
  | Investors_very_concerned -> 0
  | Investors_outraged -> 1

let show_investor = function
  | Ui_msg.Investors_ecstatic -> "ecstatic"
  | Investors_pleased -> "pleased"
  | Investors_concerned -> "concerned"
  | Investors_very_concerned -> "very concerned"
  | Investors_outraged -> "outraged"
  
