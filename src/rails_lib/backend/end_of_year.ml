open Containers

let run_checks trains params =
  let fiscal_next_year = match params.Params.fiscal_period_year with `First -> `Second | `Second -> `First in
  let ui_msgs =
    Trainmap.fold_mapi_in_place (fun idx acc train ->
      let period = Train.get_period params.fiscal_period_year train in
      let acc = if Money.(period.revenue = zero) then Ui_msg.TrainNoRevenue(idx)::acc else acc in
      let acc = if not train.had_maintenance then Ui_msg.TrainNoMaintenance(idx)::acc else acc in
      let acc = if Train.get_route_length train = 0 then Ui_msg.TrainNoSchedule(idx)::acc else acc in
      let maintenance_cost =
        let added_maint = if Train.get_engine train |> Engine.has_steam then 2 else 1 in
        let no_maint_penalty = if train.had_maintenance then 0 else 2 in
        Money.(train.maintenance_cost +~ added_maint +~ no_maint_penalty) in
      let acc = if Money.(maintenance_cost >= Train.get_engine_cost train) then Ui_msg.TrainOldEngine(idx)::acc else acc in
      let periodic = Train.update_periodic fiscal_next_year train.periodic (fun _ -> Train.make_periodic ()) in
      acc, {train with had_maintenance=false; periodic})
      trains
      ~init:[]
  in
  let num_fiscal_periods = (params.Params.year - params.year_start) / 2 in
  let fiscal_period_year = match params.fiscal_period_year with `First -> `Second | `Second -> `First in
  ()





