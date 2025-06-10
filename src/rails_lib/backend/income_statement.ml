open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives

include Income_statement_d

let combine = Money.add
let zero = Money.zero

let deduct expense money v =
  let expenses = ExpenseMap.incr_f ~combine ~zero expense money v.expenses in
  {v with expenses}

let add_revenue revenue money v =
  let revenues = RevenueMap.incr_cash revenue money v.revenues in
  {v with revenues}

let add_revenues revenues v =
  let revenues = RevenueMap.merge_add_cash revenues revenues in
  {v with revenues}

let add_expenses expenses v =
  let expenses = ExpenseMap.merge_add_cash expenses expenses in
  {v with expenses}

let merge v1 v2 =
  let revenues = RevenueMap.merge_add_cash v1.revenues v2.revenues in
  let expenses = ExpenseMap.merge_add_cash v1.expenses v2.expenses in
  {revenues; expenses}

let total_revenue v = RevenueMap.total_cash v.revenues

let total_expenses v = ExpenseMap.total_cash v.expenses

let total v = Money.(total_revenue v - total_expenses v)

