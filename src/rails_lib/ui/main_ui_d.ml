open Ppx_yojson_conv_lib.Yojson_conv.Primitives

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
  type t = news_types [@@ deriving yojson]
  let to_enum = news_types_to_enum
  let of_enum = news_types_of_enum
  let last = `Local end)

type features =
  [`Animations | `Sounds]
  [@@deriving enum, eq, show, yojson]

module Features = Bitset.Make(struct
  type t = features [@@ deriving yojson]
  let to_enum = features_to_enum
  let of_enum = features_of_enum
  let last = `Sounds
end)

type options = {
  message_speed: message_speed;
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
    | `Efficiency_report
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
    | `Cheat of Cheat_d.t
    | `Quit_game
    ]
    [@@deriving show, yojson]

    (* modalmenu type used for factoring *)
type ('state, 'menu_options, 'payload) modalmenu =
  {
    menu: ('menu_options option, 'state) Menu.MsgBox.t;
    data: 'payload;
    last: 'state mode;
  }
    (* Main modes of operation of the mapview.
       Any special menu needs a mode.
       'a: used to allow recursive modules with the main State.t
       2nd: type of choices from menu
       3rd: type of stored data
    *)
and 'state mode =
  | Normal
  | ModalMsgbox of ('state, unit, unit) modalmenu
  | Newspaper of {state: 'state Newspaper_d.t; next_mode: 'state mode}
  | BuildStation of ('state, Station.kind, unit) modalmenu
  | BuildBridge of ('state, Bridge.t, Backend.Action.msg) modalmenu
  | BuildHighGrade of ('state, [`BuildTunnel | `BuildTrack], Backend.Action.msg) modalmenu
  | BuildTunnel of ('state, bool, Backend.Action.msg * int) modalmenu
  | SignalMenu of ('state, [`Normal|`Hold|`Proceed], int * int * Dir.t) modalmenu (* x,y,dir *)
  | StationReport of int * int (* x, y *)
  | BuildTrain of [
    | `ChooseEngine
    | `AddCars of 'state Build_train_d.addcars
  ]
  | BuildIndustry of [
    | `ChooseIndustry of ('state, Tile.t, unit) modalmenu
    | `ConfirmBuild of ('state, unit, Tile.t * int * int) modalmenu
  ]
  | TrainReport of 'state Train_report_d.t
  | Stock_broker of 'state Stock_broker_d.t
  | Balance_sheet of Balance_sheet_d.t
  | Accomplishments
  | Income_statement of Balance_sheet_d.t  (* we use the stock part *)
  | EngineInfo of Engine_info.t
  | Efficiency_report
  | Animation of {state: Pani_render.t; next_mode: 'state mode}  (* Animation stores its next mode for ease of use *)
  | NewGoodDeliveryPickup of New_delivery_pickup_d.t

let is_normal_mode = function
  | Normal -> true
  | _ -> false

type 'state t = {
  dims: dims;
  options: options;
  menu: (menu_action, 'state) Menu.Global.t;
  mode: 'state mode; (* determines mode of operation *)
  train_ui_start: int; (* which train we start showing in the UI *)
  (* Top-right UI arrival message, time to display *)
  train_arrival_msgs: (Ui_msg.train_arrival_msg * int ref) list;
  mutable view: Mapview_d.t;
}



