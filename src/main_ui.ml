open Containers
open Main_ui_d

module R = Renderer

let main_menu fonts menu_h =
  let open Menu in
  let game_speed =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0 ~exclusive:(Some [])
    [
      make_entry "Frozen" @@ `Checkbox `Speed_frozen;
      make_entry "Slow" @@ `Checkbox `Speed_slow;
      make_entry "Moderate" @@ `Checkbox `Speed_moderate;
      make_entry "Fast" @@ `Checkbox `Speed_fast;
      make_entry "Turbo" @@ `Checkbox `Speed_turbo;
    ]
  in
  let train_messages =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0 ~exclusive:(Some [])
    [
      make_entry "Off" @@ `Checkbox `Message_off;
      make_entry "Fast" @@ `Checkbox `Message_fast;
      make_entry "Slow" @@ `Checkbox `Message_slow;
    ]
  in
  let news_reports =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Financial News" @@ `Checkbox `News_financial;
      make_entry "Railroad News" @@ `Checkbox `News_railroad;
      make_entry "Local News" @@ `Checkbox `News_local;
    ]
  in
  let features =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Animations" @@ `Checkbox `Features_animations;
      make_entry "Sound Effects" @@ `Checkbox `Features_sounds;
    ]
  in
  let game_menu =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Game Speed" @@ `MsgBox game_speed;
      make_entry "Train Messages" @@ `MsgBox train_messages;
      make_entry "News Reports" @@ `MsgBox news_reports;
      make_entry "Features" @@ `MsgBox features;
      make_entry "Repeat Message" @@ `Action `Repeat_message;
      make_entry "Save Game" @@ `Action `Save_game;
    ]
  in
  let reports_menu =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Balance Sheet" @@ `Action `Balance_sheet;
      make_entry "Income Statement (F5)" @@ `Action `Income_statement;
      make_entry "Train Income (F6)" @@ `Action `Train_income;
      make_entry "Stocks" @@ `Action `Stocks;
      make_entry "Accomplishments" @@ `Action `Accomplishments;
      make_entry "Efficiency" @@ `Action `Efficiency;
      make_entry "History" @@ `Action `History;
    ]
  in
  let displays_menu =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Regional Display (F1)" @@ `Action `Display_regional;
      make_entry "Area Display (F2)" @@ `Action `Display_area;
      make_entry "Local Display (F3)" @@ `Action `Display_local;
      make_entry "Detail Display (F4)" @@ `Action `Display_detail;
      make_entry "Options" @@ `Action `Options;
      make_entry "Find City" @@ `Action `Find_city;
    ]
  in
  let build_menu =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0 ~exclusive:(Some [3;4])
    [
      make_entry "New Train (F7)" @@ `Action `Build_train;
      make_entry "Build Station (F8)" @@ `Action `Build_station;
      make_entry "Build Industry" @@ `Action `Build_industry;
      make_entry "Build Track" @@ `Checkbox `Build_track;
      make_entry "Remove Track" @@ `Checkbox `Remove_track;
      make_entry "Improve Station" @@ `Action `Improve_station;
      make_entry "Upgrade Bridge" @@ `Action `Upgrade_bridge;
    ]
  in
  let reality_levels =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Dispatcher Operations" @@ `Checkbox `Option_dispatcher_ops;
      make_entry "Complex Economy" @@ `Checkbox `Option_complex_economy;
      make_entry "Cut-Throat Competition" @@ `Checkbox `Option_cutthroat;
    ]
  in
  let actions_menu =
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Call Broker (F9)" @@ `Action `Action_call_broker;
      make_entry "Survey (F10)" @@ `Action `Action_survey;
      make_entry "Name RR" @@ `Action `Action_name_rr;
      make_entry "Reality Levels" @@ `MsgBox reality_levels;
      make_entry "Retire" @@ `Action `Action_retire;
    ]
  in
  let titles =
    let open Menu.Title in
    [
      make ~fonts ~x:8 ~y:1 "&Game" game_menu;
      make ~fonts ~x:64 ~y:1 "&Displays" displays_menu;
      make ~fonts ~x:120 ~y:1 "&Reports" reports_menu;
      make ~fonts ~x:176 ~y:1 "&Build" build_menu;
      make ~fonts ~x:242 ~y:1 "&Actions" actions_menu;
    ]
  in
  Menu.Global.make ~menu_h titles

let default win fonts =
  let dims =
    {
      menu_h=8;
      minimap_h=55;
      width=R.width win;
      height=R.height win;
      ui_w=64;
      ui_start_x=Gmap.map_width-1;
      infobar_h=19;
      train_area_h=115;
    }
  in
  {
    dims;
    menu=main_menu fonts dims.menu_h;
  }

let update (s:State.t) (v:t) (event:Event.t) =
  let dims = v.dims in
  let view, action =
    (* only update view if we have a change *)
    match Menu.Global.update v.menu event with
    | _, Menu.NoAction -> v, Menu.NoAction
    | menu, a -> {v with menu}, a
  in
  let minimap_x = dims.ui_start_x in
  let minimap_y = dims.menu_h in
  let minimap_h = dims.minimap_h in
  let minimap_w = dims.ui_w in
  let view, actions =
    Mapview.update s s.view event ~y_top:dims.menu_h ~minimap_x ~minimap_y ~minimap_w ~minimap_h
  in
  v, view, actions
  

let render (win:R.window) (s:State.t) (v:t) =
  let dims = v.dims in
  (* Render main view *)
  let minimap_x = dims.ui_start_x in
  let minimap_y = dims.menu_h in
  let minimap_h = dims.minimap_h in
  let minimap_w = dims.ui_w in
  let s = Mapview.render win s s.view ~y:v.dims.menu_h ~minimap_x ~minimap_y ~minimap_w ~minimap_h in

  (* Menu bar *)
  R.draw_rect win ~x:0 ~y:0 ~w:dims.width ~h:dims.menu_h ~color:Ega.cyan ~fill:true;
  let h = dims.height - dims.menu_h in
  let y = dims.menu_h in

  (* Screen White border *)
  R.draw_rect win ~x:0 ~y ~w:dims.width ~h ~color:Ega.white ~fill:false;

  let x = dims.ui_start_x in

  (* Border of UI *)
  R.draw_rect win ~x ~y ~h ~w:(dims.ui_w+1) ~color:Ega.white ~fill:false;

  begin match Mapview.get_zoom s.view with
  | Zoom1 ->
      R.Texture.render ~x:(x+1) ~y:(y+1) win s.State.textures.Textures.logo;
  | _ ->
      ()
  end;

  (* Info bar *)
  let y = y + dims.minimap_h in
  R.draw_rect win ~x ~y ~h:dims.infobar_h ~w:dims.ui_w ~color:Ega.white ~fill:true;

  (* Train area *)
  let y = y + dims.infobar_h in
  R.draw_rect win ~x:(x+1) ~y:(y+1) ~h:dims.train_area_h ~w:(dims.ui_w-1) ~color:Ega.bblue ~fill:true;

  (* Menu bar *)
  Menu.Global.render win s.textures.fonts v.menu;

  s

