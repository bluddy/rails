
type menu_action =
  [
    | `Speed_frozen
    | `Speed_slow
    | `Speed_moderate
    | `Speed_fast
    | `Speed_turbo
    | `Message_off
    | `Message_fast
    | `Message_slow
    | `News_financial
    | `News_railroad
    | `News_local
    | `Features_animations
    | `Features_sounds
    | `Repeat_message
    | `Save_game
    | `Balance_sheet
    | `Income_statement
    | `Train_income
    | `Stocks
    | `Accomplishments
    | `Efficiency
    | `History
    | `Display_regional
    | `Display_area
    | `Display_local
    | `Display_detail
    | `Options
    | `Find_city
    | `Build_train
    | `Build_station
    | `Build_industry
    | `Build_track
    | `Remove_track
    | `Improve_station
    | `Upgrade_bridge
    | `Option_dispatcher_ops
    | `Option_complex_economy
    | `Option_cutthroat
    | `Action_call_broker
    | `Action_survey
    | `Action_name_rr
    | `Action_retire
    ]

type dims = {
  menu_h: int;
  ui_w: int;
  ui_start_x: int;
  minimap_h: int;
  infobar_h: int;
  train_area_h: int;
  width: int;
  height: int;
}

type t = {
  dims: dims;
  menu: menu_action Menu.Global.t;
}
