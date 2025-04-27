(* Module for stockmarket *)
open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants

module IntMap = Utils.IntMap

type t = {
  (* Who owns how many shares in what company. By default, all shares are public
     First level: owner. Second level: ownee
     *)
  ownership: (int IntMap.t) IntMap.t;
  prices: int IntMap.t; (* share prices *)
  totals: int IntMap.t; (* total number of shares *)
}

let default = {
  ownership=IntMap.empty;
  prices=IntMap.empty;
  totals=IntMap.empty;
}

let player_starting_share_price difficulty = 
    B_options.difficulty_to_enum difficulty + 7

let add_human_player ~player difficulty v =
  let totals = IntMap.add player C.Stock.starting_num v.totals in
  let prices = IntMap.add player (B_options.difficulty_to_enum difficulty + 7) v.prices in
  {v with totals; prices}

let owned_shares ~owner ~ownee v =
  match IntMap.get ~owner v.ownership with
  | Some ownees -> 
    Intmap.get_or ~default:0 ownee ownees
  | None -> 0

let self_owned_shares player v = owned_shares ~owner:player ~ownee:player v

let treasury_shares player v = owned_shares ~owner:player ~ownee:player v

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
  compute_owned_share_value ~total_shares ~owned_shares:treasury_shares ~share_price

let add_shares ~owner ~ownee ~num v =
  let ownership =
    IntMap.update owner (function
      | Some ownees -> IntMap.incr ownee num ownees |> Option.some
      | None -> IntMap.empty |> IntMap.incr ownee num |> Option.some)
    v.ownership
  in
  {v with ownership}

let remove_shares ~owner ~ownee ~num v = add_shares ~owner ~ownee ~num:(-num) v

let set_shares ~owner ~ownee ~num v =
  let ownership =
    IntMap.update owner (function
      | Some ownees -> IntMap.add ownee num ownees |> Option.some
      | None -> IntMap.empty |> IntMap.add ownee num |> Option.some)
    v.ownership
  in
  {v with ownership}

let reset_owned_shares player v =
  let ownership = IntMap.remove player v.ownership in
  {v with ownership}

let set_total_shares ~player total_shares v =
  let totals = IntMap.add player total_shares v.totals in
  {v with totals}

let set_share_price ~player ~price v =
  let prices = IntMap.add player price v.prices in
  {v with prices}

let num_other_companies_owner_has_shares_in owner v =
  match IntMap.get owner v.ownership with
  | None -> 0
  | Some ownees ->
    IntMap.sum (fun player shares ->
      if owner <> player && shares > 0 then 1 else 0)
      ownees

let shares_owned_by_all_companies player ?exclude v =
  IntMap.sum (fun owner ownees ->
     match exclude with
     | Some exclude when owner = exclude -> 0
     | _ ->
        IntMap.sum (fun ownee shares -> if ownee = player then shares else 0) ownees
  ) v.ownership

let shares_owned_by_other_companies player v =
  shares_owned_by_all_companies player ~exclude:player v

  (* Publicly available (unowned) shares *)
let public_shares player v =
  let owned_shares = shares_owned_by_all_companies player v in
  total_shares player v - owned_shares

let has_controlling_stake player ~target v =
  let owned_shares = owned_shares ~owner:player ~ownee:target v in
  let total_shares = total_shares target v in
  (* TODO: is this geq or gt? *)
  owned_shares > total_shares / 2

let has_self_controlling_stake player v = has_controlling_stake player ~target:player v

let check_can_buy_stock ~player ~target ~cash ~difficulty v =
  (* TODO: In original code, it's < total_shares - 10. Not sure why *)
  (* Test if we have an 'anti-trust' problem *)
  let max_owned_companies = Utils.clip (B_options.difficulty_to_enum difficulty) ~min:1 ~max:3 in
  let public_shares = public_shares target v in
  let v2 = add_shares ~owner:player ~ownee:target ~num:C.num_buy_shares v in
  if num_other_companies_owner_has_shares_in player v2 > max_owned_companies then
    `Anti_trust_violation max_owned_companies
    (* TODO: double check the below buying out *)
  else if player <> target &&
       public_shares = 0 &&
       Stocks.owned_shares v.m.stock target_idx < tgt_player.m.stock.total_shares / 2
    then
      let share_price = share_price target v * 2 in
      let shares_to_buy = shares_owned_by_all_companies target ~exclude:player in
      `Offer_takeover(share_price, shares_to_buy)
  else
    (* TODO: what about buying the rest of your own shares? *)
    let share_price = share_price target v in
    let can_buy = Stocks.owned_shares v.m.stock target_idx < get_total_shares tgt_player in
    let cost = share_price * C.num_buy_shares in
    if cash >= cost && can_buy then `Ok
  else `Error

let _sell_buy_calculations v target_player ~target_idx ~add_stock ~buy =
  let share_price = target_player.m.stock.share_price in
  let non_treasury_shares = Stocks.non_treasury_shares target_player.m.stock + add_stock in
  let cost = share_price * C.num_buy_shares in
  let price_change =
    let delta = if buy then 1 else 0 in
    (cost / non_treasury_shares) + delta
  in
  let price_change = if buy then price_change else -price_change in
  Log.debug (fun f -> f "price change[%d] non-t-shares[%d] share_price=[%d]" price_change non_treasury_shares share_price);
  let share_price2 = share_price + price_change in
  let num_shares = if buy then C.num_buy_shares else -C.num_buy_shares in
  let stock = Stocks.add_shares v.m.stock ~target_idx ~num_shares in
  let cost = if buy then -cost else cost in
  let cash = v.m.cash + cost in
  cash, stock, cost, share_price2

let buy_stock players ~player_idx ~target_idx ~difficulty =
  let v = get_player players player_idx in
  let tgt_player = get_player players target_idx in
  match can_buy_stock players ~target_idx ~player_idx ~difficulty with
  | `Ok when player_idx = target_idx ->
    (* add_stock: code adds 10 for ai *)
    let cash, stock, cost, share_price = _sell_buy_calculations v tgt_player ~target_idx ~add_stock:0 ~buy:true in
    let v = {v with m={v.m with cash; stock={stock with share_price}}} in
    players.(player_idx) <- v;
    `Bought(cost)

  | `Ok -> (* different companies *)
    let cash, stock, cost, share_price = _sell_buy_calculations v tgt_player ~target_idx ~add_stock:C.num_buy_shares ~buy:true in
    let v = {v with m={v.m with cash; stock}} in
    players.(player_idx) <- v;
    players.(target_idx) <- set_share_price players.(target_idx) share_price;
    `Bought(cost)

  | `Offer_takeover(share_price, num_shares) when get_cash v >= share_price * num_shares -> (* buy all *)
    (* Buy all stock for one player. Remove from all others *)
    Array.mapi_inplace (fun i player ->
      let stock, cash = if i = player_idx then
        Stocks.add_shares player.m.stock ~target_idx ~num_shares,
        (get_cash player) - (share_price * num_shares)
      else
        let num_shares = Stocks.owned_shares player.m.stock target_idx in
        Stocks.set_shares player.m.stock ~target_idx ~num_shares:0,
        player.m.cash + num_shares * share_price
      in
      {player with m={player.m with stock; cash}}
    ) players;
    `Takeover

  | _ -> `None

let can_sell_stock (v:t) target_idx =   
  Stocks.owned_shares v.m.stock target_idx > 0

let sell_stock (v:t) target_player player_idx ~target_idx =
  match can_sell_stock v target_idx with
  | true when player_idx = target_idx ->
    let cash, stock, cost, share_price = _sell_buy_calculations v target_player ~target_idx ~add_stock:0 ~buy:false in
    Some({v with m={v.m with cash; stock={stock with share_price}}}, cost, None)

  | true ->
    let cash, stock, cost, share_price = _sell_buy_calculations v target_player ~target_idx ~add_stock:C.num_buy_shares ~buy:false in
    Some({v with m={v.m with cash; stock}}, cost, Some share_price)

  | false -> None

let owns_company player ~company ~company_idx =
  Stocks.owned_shares player.m.stock company_idx > Stocks.total_shares company.m.stock / 2

let owns_company_by_idx (players:t array) ~player_idx ~company_idx =
  let player = players.(player_idx) in
  let company = players.(company_idx) in
  owns_company player ~company ~company_idx

let companies_controlled_by (players:t array) ~player_idx =
  let player = players.(player_idx) in
  Array.foldi (fun acc i company ->
    if i <> player_idx && owns_company player ~company ~company_idx:i
    then i::acc else acc)
  []
  players

let owns_some_company (players: t array) ~player_idx =
  let player = players.(player_idx) in
  Array.foldi (fun acc i company ->
    if i <> player_idx && owns_company player ~company ~company_idx:i
    then acc || true else false)
  false
  players

  (* Compute value of all stocks a player has in all companies, including itself *)
let total_owned_stock_value (players:t array) ~player_idx =
  let player = players.(player_idx) in
  Array.foldi (fun acc i company ->
    acc + ((Stocks.owned_shares player.m.stock i) / 10) * Stocks.share_price company.m.stock)
  0
  players
