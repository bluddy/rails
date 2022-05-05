open Containers
open Main_ui_d

module R = Renderer
module B = Backend

(* Create menu *)
let main_menu fonts menu_h =
  let open Menu in
  let game_speed =
    let check_speed speed (s:State.t) = B.equal_speed s.backend.options.speed speed in
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Frozen" @@ `Checkbox(`Speed `Frozen, check_speed `Frozen);
      make_entry "Slow" @@ `Checkbox(`Speed `Slow, check_speed `Slow);
      make_entry "Moderate" @@ `Checkbox(`Speed `Moderate, check_speed `Moderate);
      make_entry "Fast" @@ `Checkbox(`Speed `Fast, check_speed `Fast);
      make_entry "Turbo" @@ `Checkbox(`Speed `Turbo, check_speed `Turbo);
    ]
  in
  let train_messages =
    let check_message message (s:State.t) = Main_ui_d.equal_message_speed s.ui.options.messages message in
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Off" @@ `Checkbox(`Message `Off, check_message `Off);
      make_entry "Fast" @@ `Checkbox(`Message `Fast, check_message `Fast);
      make_entry "Slow" @@ `Checkbox(`Message `Slow, check_message `Slow);
    ]
  in
  let news_reports =
    let check_news news_type (s:State.t) =
      Main_ui_d.NewsTypes.mem s.ui.options.news news_type
    in
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Financial News" @@ `Checkbox(`News `Financial, check_news `Financial);
      make_entry "Railroad News" @@ `Checkbox(`News `Railroad, check_news `Railroad);
      make_entry "Local News" @@ `Checkbox(`News `Local, check_news `Local);
    ]
  in
  let features =
    let check_feature feature (s:State.t) =
      Main_ui_d.Features.mem s.ui.options.features feature
    in
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Animations" @@ `Checkbox(`Features `Animations, check_feature `Animations);
      make_entry "Sound Effects" @@ `Checkbox(`Features `Sounds, check_feature `Sounds);
    ]
  in
  let game_menu =
    let open MsgBox in
    make ~fonts ~x:4 ~y:8
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
    make ~fonts ~x:112 ~y:8
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
    make ~fonts ~x:56 ~y:8
    [
      make_entry "Regional Display (F1)" @@ `Action(`Display(Mapview_d.Zoom1));
      make_entry "Area Display (F2)" @@ `Action(`Display(Mapview_d.Zoom2));
      make_entry "Local Display (F3)" @@ `Action(`Display(Mapview_d.Zoom3));
      make_entry "Detail Display (F4)" @@ `Action(`Display(Mapview_d.Zoom4));
      make_entry "Options" @@ `Action `Options;
      make_entry "Find City" @@ `Action `Find_city;
    ]
  in
  let is_zoom4 = Some (fun (s:State.t) -> Mapview.is_zoom4 s.view) in
  let is_station4 =
    (fun (s:State.t) -> Mapview.is_zoom4 s.view && Mapview.cursor_on_station s)
    |> Option.return
  in
  let is_woodbridge4 =
    (fun (s:State.t) -> Mapview.is_zoom4 s.view && Mapview.cursor_on_woodbridge s)
    |> Option.return
  in
  let build_menu =
    let check_trackbuild build (s:State.t) = Mapview_d.equal_build_mode s.view.build_mode build in
    let open MsgBox in
    make ~fonts ~x:168 ~y:8
    [
      make_entry "New Train (F7)" @@ `Action `Build_train;
      make_entry "Build Station (F8)" ~visibility:is_zoom4 @@ `Action `Build_station;
      make_entry "Build Industry" ~visibility:is_zoom4 @@ `Action `Build_industry;
      make_entry "Build Track" ~visibility:is_zoom4 @@ `Checkbox(`Track `Build, check_trackbuild `Build);
      make_entry "Remove Track" ~visibility:is_zoom4 @@ `Checkbox(`Track `Remove, check_trackbuild `Remove);
      make_entry "Improve Station" ~visibility:is_station4 @@ `Action `Improve_station;
      make_entry "Upgrade Bridge" ~visibility:is_woodbridge4 @@ `Action `Upgrade_bridge;
    ]
  in
  let reality_levels =
    let check_reality level (s:State.t) =
      Backend.RealityLevels.mem s.backend.options.reality_levels level
    in
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "Dispatcher Operations" @@ `Checkbox(`Reality_level `Dispatcher_ops, check_reality `Dispatcher_ops);
      make_entry "Complex Economy" @@ `Checkbox(`Reality_level `Complex_economy, check_reality `Complex_economy);
      make_entry "Cut-Throat Competition" @@ `Checkbox(`Reality_level `Cutthroat_competition, check_reality `Cutthroat_competition);
    ]
  in
  let actions_menu =
    let open MsgBox in
    make ~fonts ~x:202 ~y:8
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
      train_area_h=117;
    }
  in
  let options =
    {
      messages=`Slow;
      news=NewsTypes.of_list [`Financial; `Railroad; `Local];
      features=Features.of_list [`Animations; `Sounds];
    }
  in
  {
    dims;
    menu=main_menu fonts dims.menu_h;
    options;
  }

let update (s:State.t) v (event:Event.t) =
  let dims = v.dims in
  let v, action, event =
    (* only update view if we have a change *)
    match Menu.Global.update s v.menu event with
    | _, Menu.NoAction -> v, Menu.NoAction, event
    | menu, a -> {v with menu}, a, NoEvent
  in
  let minimap_x = dims.ui_start_x in
  let minimap_y = dims.menu_h in
  let minimap_h = dims.minimap_h in
  let minimap_w = dims.ui_w in
  let view, actions =
    Mapview.update s s.view event ~y_top:dims.menu_h ~minimap_x ~minimap_y ~minimap_w ~minimap_h
  in
  v, view, actions
  

let render (win:R.window) (s:State.t) v =
  let dims = v.dims in
  (* Render main view *)
  let minimap_x = dims.ui_start_x in
  let minimap_y = dims.menu_h in
  let minimap_h = dims.minimap_h in
  let minimap_w = dims.ui_w in
  let s = Mapview.render win s s.view ~y:v.dims.menu_h ~minimap_x ~minimap_y ~minimap_w ~minimap_h in

  (* Menu bar background *)
  R.draw_rect win ~x:0 ~y:0 ~w:dims.width ~h:dims.menu_h ~color:Ega.cyan ~fill:true;
  let h = dims.height - dims.menu_h in
  let y = dims.menu_h in

  (* Screen White border *)
  R.draw_rect win ~x:0 ~y ~w:dims.width ~h ~color:Ega.white ~fill:false;

  let x = dims.ui_start_x in

  (* Border of UI *)
  R.draw_rect win ~x ~y ~h ~w:(dims.ui_w+1) ~color:Ega.white ~fill:false;

  (* Draw logo *)
  begin match Mapview.get_zoom s.view with
  | Zoom1 ->
      R.Texture.render ~x:(x+1) ~y:(y+1) win s.State.textures.Textures.logo;
  | _ -> ()
  end;

  (* Info bar *)
  let y = y + dims.minimap_h in
  R.draw_rect win ~x ~y ~h:dims.infobar_h ~w:dims.ui_w ~color:Ega.white ~fill:true;

  (* Train area *)
  let y = y + dims.infobar_h in
  R.draw_rect win ~x:(x+1) ~y:y ~h:dims.train_area_h ~w:(dims.ui_w-1) ~color:Ega.bblue ~fill:true;

  (* Menu bar *)
  Menu.Global.render win s s.textures.fonts v.menu;

  s

