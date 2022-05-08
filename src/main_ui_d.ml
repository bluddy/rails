

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

type message_speed =
  [`Slow | `Fast | `Off]
  [@@deriving eq, show]

type news_types =
  [`Financial | `Railroad | `Local]
  [@@deriving enum, eq, show]

module NewsTypes = Bitset.Make(struct
  type t = news_types
  let to_enum = news_types_to_enum
  let of_enum = news_types_of_enum
  let last = `Local end)

type features =
  [`Animations | `Sounds]
  [@@deriving enum, eq, show]

module Features = Bitset.Make(struct
  type t = features
  let to_enum = features_to_enum
  let of_enum = features_of_enum
  let last = `Sounds end)

type options = {
  messages: message_speed;
  news: NewsTypes.t;
  features: Features.t;
}

type menu_action =
  [
    | `Speed of Backend.speed
    | `Message of message_speed
    | `News of news_types
    | `Features of features
    | `Repeat_message
    | `Save_game
    | `Balance_sheet
    | `Income_statement
    | `Train_income
    | `Stocks
    | `Accomplishments
    | `Efficiency
    | `History
    | `Display of Mapview_d.zoom
    | `Options of Mapview_d.options
    | `Find_city
    | `Build_train
    | `Build_station
    | `Build_industry
    | `Track of Mapview_d.build_mode
    | `Remove_track
    | `Improve_station
    | `Upgrade_bridge
    | `Reality_level of Backend.reality_level
    | `Action_call_broker
    | `Action_survey
    | `Action_name_rr
    | `Action_retire
    ]
    [@@deriving show]

type 'a mode =
  | Normal
  | BuildStation of (Station.t, 'a) Menu.MsgBox.t

type 'a t = {
  dims: dims;
  options: options;
  menu: (menu_action, 'a) Menu.Global.t;
  mode: 'a mode; (* determines mode of operation *)
}


