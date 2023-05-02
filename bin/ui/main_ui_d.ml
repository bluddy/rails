
type dims = {
  screen: Utils.rect;
  menu: Utils.rect;
  mapview: Utils.rect;
  ui: Utils.rect;
  minimap: Utils.rect;
  infobar: Utils.rect;
  train_ui: Utils.rect;
  train_ui_train_h: int;
} [@@deriving yojson]

type message_speed =
  [`Slow | `Fast | `Off]
  [@@deriving eq, show, yojson]

type news_types =
  [`Financial | `Railroad | `Local]
  [@@deriving enum, eq, show, yojson]

module NewsTypes = Bitset.Make(struct
  type t = news_types
  let to_enum = news_types_to_enum
  let of_enum = news_types_of_enum
  let last = `Local end)

type features =
  [`Animations | `Sounds]
  [@@deriving enum, eq, show, yojson]

module Features = Bitset.Make(struct
  type t = features
  let to_enum = features_to_enum
  let of_enum = features_of_enum
  let last = `Sounds
end)

type options = {
  messages: message_speed;
  news: NewsTypes.t;
  features: Features.t;
} [@@deriving yojson]

type menu_action =
  [
    | `Speed of B_options.speed
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
    | `BuildTrack
    | `RemoveTrack
    | `ImproveStation of Station.upgrade
    | `Upgrade_bridge
    | `Reality_level of B_options.reality_level
    | `Call_broker
    | `Survey
    | `Name_rr
    | `Retire
    ]
    [@@deriving show, yojson]

    (* modalmenu type used for factoring *)
type ('a, 'b, 'c) modalmenu =
  {
    menu: ('b option, 'a) Menu.MsgBox.t;
    data: 'c;
    last: 'a mode;
  }
    (* Main modes of operation of the mapview.
       Any special menu needs a mode.
       'a: used to allow recursive modules with the main State.t
    *)
and 'a mode =
  | Normal
  | ModalMsgbox of ('a, unit, unit) modalmenu
  | BuildStation of ('a, Station.kind, unit) modalmenu
  | BuildBridge of ('a, Bridge.t, Utils.msg) modalmenu
  | BuildTunnel of ('a, [`Tunnel | `Track], (Utils.msg * int)) modalmenu
  | StationView of int * int (* x, y *)
  | BuildTrain of [
    | `ChooseEngine
    | `AddCars of 'a Build_train_d.addcars
  ]
  | EditTrain of 'a Edit_train_d.t

type 'state t = {
  dims: dims;
  options: options;
  menu: (menu_action, 'state) Menu.Global.t;
  mode: 'state mode; (* determines mode of operation *)
  train_ui_start: int; (* which train we start showing in the UI *)
  mutable view: Mapview_d.t;
}



