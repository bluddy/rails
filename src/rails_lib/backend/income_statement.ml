open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module Hashtbl = Utils.Hashtbl

open Income_statement_d

let deduct expense money v =
  let expenses = ExpenseMap.add_amount expense money v.expenses in
  {v with expenses}

let add_revenue revenue money v =
  let revenues = RevenueMap.add_amount revenue money v.revenues in
  {v with revenues}

let add_revenues revenues v =
  let revenues = RevenueMap.merge_add v.revenues revenues in
  {v with revenues}

let add_expenses expenses v =
  let expenses = ExpenseMap.merge_add v.expenses expenses in
  {v with expenses}

let total_revenue v = RevenueMap.fold (fun _ i acc -> i + acc) v.revenues 0

