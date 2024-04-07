open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl

open Income_statement_d

let deduct expense money v =
  let expenses = ExpenseMap.update expense 
    (Option.map @@ fun current -> current + money) v.expenses in
  {v with expenses}

let add_revenue revenue money v =
  let revenues = RevenueMap.update revenue
    (Option.map @@ fun current -> current + money) v.revenues in
  {v with revenues}

let total_revenue v = RevenueMap.fold (fun _ i acc -> i + acc) v.revenues 0

