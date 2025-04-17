open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

include Income_statement_d

let deduct expense money v =
  let expenses = ExpenseMap.incr expense money v.expenses in
  {v with expenses}

let add_revenue revenue money v =
  let revenues = RevenueMap.incr revenue money v.revenues in
  {v with revenues}

let add_revenues revenues v =
  let revenues = RevenueMap.merge_add v.revenues revenues in
  {v with revenues}

let add_expenses expenses v =
  let expenses = ExpenseMap.merge_add v.expenses expenses in
  {v with expenses}

let merge v1 v2 =
  let revenues = RevenueMap.merge_add v1.revenues v2.revenues in
  let expenses = ExpenseMap.merge_add v1.expenses v2.expenses in
  {revenues; expenses}

let total_revenue v = RevenueMap.total v.revenues

let total_expenses v = ExpenseMap.total v.expenses

let total v = total_revenue v - total_expenses v

