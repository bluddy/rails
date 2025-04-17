open! Containers
module C = Constants
module R = Renderer

open Income_statement_d

let relevant_expenses = [
  `InterestFees; `TrainMaintenance; `TrackMaintenance; `StationMaintenance;
]

let render win (s:State.t) balance_sheet =
  let player = Backend.get_player s.backend C.player in
  let is, old_is = player.m.income_statement, player.m.total_income_statement in
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  Fonts.Render.write_shadow win s.fonts ~idx:2 ~color:Ega.gray ~x:16 ~y:4 @@
    Printf.sprintf "Income Statement: %d" s.backend.year;
  let climate = Climate.show s.backend.climate in
  Fonts.Render.write win s.fonts ~idx:4 ~color:Ega.gray ~x:80 ~y:23 @@
    Printf.sprintf "Economic Climate: %s" climate;

  let x_left, x_text, x_ytd, x_total = 16, 20, 160, 240 in
  let write ?(color=Ega.black) = Fonts.Render.write win s.fonts ~idx:4 ~color in
  let write_money ~x ~y money =
    let money_s = Utils.show_cash ~show_neg:false ~spaces:6 ~region:s.backend.region money in
    let color = if money < 0 then Ega.bred else Ega.black in
    write ~x ~y ~color money_s
  in
  let write_total_and_ytd ?text ~y money oldval =
    Option.iter (write ~x:x_text ~y) text;
    write_money ~x:x_ytd ~y money;
    write_money ~x:x_total ~y oldval
  in
  let write_rev_total_and_ytd ~y revenue =
    let rev_s = show_revenue revenue in
    write ~x:x_text ~y rev_s;
    let ytd_money = RevenueMap.find_opt revenue is.revenues |> Option.get_or ~default:0 in
    write_money ~x:x_ytd ~y ytd_money;
    let money = RevenueMap.find_opt revenue old_is.revenues |> Option.get_or ~default:0 in
    write_money ~x:x_total ~y money
  in
  let write_exp_total_and_ytd ~y expense =
    let exp_s = show_expense expense in
    write ~x:x_text ~y exp_s;
    let ytd_money = ExpenseMap.find_opt expense is.expenses |> Option.get_or ~default:0 in
    write_money ~x:x_ytd ~y ytd_money;
    let money = ExpenseMap.find_opt expense old_is.expenses |> Option.get_or ~default:0 in
    write_money ~x:x_total ~y money
  in
  let revenue_sum is =
    RevenueMap.fold (fun _ money acc -> acc + money) is 0
  in
  let expense_sum is =
    ExpenseMap.fold (fun expense money acc ->
      if List.mem ~eq:equal_expense expense relevant_expenses then acc + money else acc)
    is
    0
  in
  let line = 8 in
  let y = 36 in
  write ~x:x_left ~y "REVENUES:";
  write ~x:x_ytd ~y "YTD:";
  write ~x:x_total ~y "Total:";
  let y = y + 12 in
  write_rev_total_and_ytd ~y `Mail;
  let y = y + line in
  write_rev_total_and_ytd ~y `Passenger;
  let y = y + line in
  write_rev_total_and_ytd ~y `Fast;
  let y = y + line in
  write_rev_total_and_ytd ~y `Slow;
  let y = y + line in
  write_rev_total_and_ytd ~y `Bulk;
  let y = y + line in
  write_rev_total_and_ytd ~y `Other;
  let y = y + line in
  R.draw_line win ~x1:x_ytd ~y1:y ~x2:(x_ytd+54) ~y2:y ~color:Ega.black;
  R.draw_line win ~x1:x_total ~y1:y ~x2:(x_total+54) ~y2:y ~color:Ega.black;
  let y = y + 2 in
  let rev_sum = revenue_sum is.revenues in
  let rev_sum_old = revenue_sum old_is.revenues in
  write_total_and_ytd ~y rev_sum rev_sum_old;

  let y = y + line in
  write ~x:x_left ~y "EXPENSES:";
  let y = y + 12 in
  write_exp_total_and_ytd ~y `InterestFees;
  let y = y + line in
  write_exp_total_and_ytd ~y `TrainMaintenance;
  let y = y + line in
  write_exp_total_and_ytd ~y `TrackMaintenance;
  let y = y + line in
  write_exp_total_and_ytd ~y `StationMaintenance;
  let y = y + line in
  R.draw_line win ~x1:x_ytd ~y1:y ~x2:(x_ytd+54) ~y2:y ~color:Ega.black;
  R.draw_line win ~x1:x_total ~y1:y ~x2:(x_total+54) ~y2:y ~color:Ega.black;
  let y = y + 2 in
  let exp_sum = expense_sum is.expenses in
  let exp_sum_old = expense_sum old_is.expenses in
  write_total_and_ytd ~y exp_sum exp_sum_old;

  let y = y + line + 2 in
  R.draw_line win ~x1:x_ytd ~y1:y ~x2:(x_ytd+54) ~y2:y ~color:Ega.black;
  R.draw_line win ~x1:x_total ~y1:y ~x2:(x_total+54) ~y2:y ~color:Ega.black;
  let y = y + 2 in
  let profit = rev_sum - exp_sum in
  let profit_old = rev_sum_old - exp_sum_old in
  write_total_and_ytd ~y ~text:"Operating Profit" profit profit_old;

  let y = y + line + 1 in
  write ~x:x_text ~y "Stock Profits:";
  let prev_balance_sheet = player.m.last_balance_sheet in
  let prev_stock = prev_balance_sheet.treasury_stock + prev_balance_sheet.other_rr_stock in
  let cur_stock = balance_sheet.Balance_sheet.treasury_stock + balance_sheet.other_rr_stock in
  let profit = cur_stock - prev_stock in
  write_money ~x:x_ytd ~y profit;
  ()
