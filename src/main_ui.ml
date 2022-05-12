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
    make ~fonts
    [
      make_entry "&Frozen" @@ `Checkbox(`Speed `Frozen, check_speed `Frozen);
      make_entry "&Slow" @@ `Checkbox(`Speed `Slow, check_speed `Slow);
      make_entry "&Moderate" @@ `Checkbox(`Speed `Moderate, check_speed `Moderate);
      make_entry "Fas&t" @@ `Checkbox(`Speed `Fast, check_speed `Fast);
      make_entry "T&urbo" @@ `Checkbox(`Speed `Turbo, check_speed `Turbo);
    ]
  in
  let train_messages =
    let check_message message (s:State.t) = Main_ui_d.equal_message_speed s.ui.options.messages message in
    let open MsgBox in
    make ~fonts
    [
      make_entry "&Off" @@ `Checkbox(`Message `Off, check_message `Off);
      make_entry "&Fast" @@ `Checkbox(`Message `Fast, check_message `Fast);
      make_entry "&Slow" @@ `Checkbox(`Message `Slow, check_message `Slow);
    ]
  in
  let news_reports =
    let check_news news_type (s:State.t) =
      Main_ui_d.NewsTypes.mem s.ui.options.news news_type
    in
    let open MsgBox in
    make ~fonts
    [
      make_entry "&Financial News" @@ `Checkbox(`News `Financial, check_news `Financial);
      make_entry "&Railroad News" @@ `Checkbox(`News `Railroad, check_news `Railroad);
      make_entry "&Local News" @@ `Checkbox(`News `Local, check_news `Local);
    ]
  in
  let features =
    let check_feature feature (s:State.t) =
      Main_ui_d.Features.mem s.ui.options.features feature
    in
    let open MsgBox in
    make ~fonts
    [
      make_entry "&Animations" @@ `Checkbox(`Features `Animations, check_feature `Animations);
      make_entry "&Sound Effects" @@ `Checkbox(`Features `Sounds, check_feature `Sounds);
    ]
  in
  let game_menu =
    let open MsgBox in
    make ~fonts ~x:4 ~y:8
    [
      make_entry "&Game Speed" @@ `MsgBox game_speed;
      make_entry "&Train Messages" @@ `MsgBox train_messages;
      make_entry "&News Reports" @@ `MsgBox news_reports;
      make_entry "&Features" @@ `MsgBox features;
      make_entry "&Repeat Message" @@ `Action `Repeat_message;
      make_entry "&Save Game" @@ `Action `Save_game;
    ]
  in
  let reports_menu =
    let open MsgBox in
    make ~fonts ~x:112 ~y:8
    [
      make_entry "&Balance Sheet" @@ `Action `Balance_sheet;
      make_entry "&Income Statement (F5)" @@ `Action `Income_statement;
      make_entry "&Train Income (F6)" @@ `Action `Train_income;
      make_entry "&Stocks" @@ `Action `Stocks;
      make_entry "&Accomplishments" @@ `Action `Accomplishments;
      make_entry "&Efficiency" @@ `Action `Efficiency;
      make_entry "&History" @@ `Action `History;
    ]
  in
  let options =
    let check_option opt (s:State.t) =
      Mapview_d.Options.mem s.ui.view.options opt
    in
    let open MsgBox in
    make ~fonts
    [
      make_entry "&Station Boxes" @@ `Checkbox(`Options `StationBoxes, check_option `StationBoxes);
      make_entry "&Resources" @@ `Checkbox(`Options `Resources, check_option `Resources);
    ]
  in
  let displays_menu =
    let open MsgBox in
    make ~fonts ~x:56 ~y:8
    [
      make_entry "&Regional Display (F1)" @@ `Action(`Display(Mapview_d.Zoom1));
      make_entry "&Area Display (F2)" @@ `Action(`Display(Mapview_d.Zoom2));
      make_entry "&Local Display (F3)" @@ `Action(`Display(Mapview_d.Zoom3));
      make_entry "&Detail Display (F4)" @@ `Action(`Display(Mapview_d.Zoom4));
      make_entry "&Options" @@ `MsgBox options;
      make_entry "&Find City" @@ `Action `Find_city;
    ]
  in
  let is_zoom4 = Some (fun (s:State.t) -> Mapview.is_zoom4 s.ui.view) in
  let is_station4 =
    (fun (s:State.t) -> Mapview.is_zoom4 s.ui.view && Mapview.cursor_on_station s.backend s.ui.view)
    |> Option.return
  in
  let is_woodbridge4 =
    (fun (s:State.t) -> Mapview.is_zoom4 s.ui.view && Mapview.cursor_on_woodbridge s.backend s.ui.view)
    |> Option.return
  in
  let build_menu =
    let check_trackbuild build (s:State.t) = Mapview_d.equal_build_mode s.ui.view.build_mode build in
    let open MsgBox in
    make ~fonts ~x:168 ~y:8
    [
      make_entry "New &Train (F7)" @@ `Action `Build_train;
      make_entry "Build &Station (F8)" ~test_enabled:is_zoom4 @@ `Action `Build_station;
      make_entry "Build &Industry" ~test_enabled:is_zoom4 @@ `Action `Build_industry;
      make_entry "&Build Track" ~test_enabled:is_zoom4 @@ `Checkbox(`Track `Build, check_trackbuild `Build);
      make_entry "&Remove Track" ~test_enabled:is_zoom4 @@ `Checkbox(`Track `Remove, check_trackbuild `Remove);
      make_entry "Im&prove Station" ~test_enabled:is_station4 @@ `Action `Improve_station;
      make_entry "Up&grade Bridge" ~test_enabled:is_woodbridge4 @@ `Action `Upgrade_bridge;
    ]
  in
  let reality_levels =
    let check_reality level (s:State.t) =
      Backend.RealityLevels.mem s.backend.options.reality_levels level
    in
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "&Dispatcher Operations" @@ `Checkbox(`Reality_level `Dispatcher_ops, check_reality `Dispatcher_ops);
      make_entry "Complex &Economy" @@ `Checkbox(`Reality_level `Complex_economy, check_reality `Complex_economy);
      make_entry "&Cut-Throat Competition" @@ `Checkbox(`Reality_level `Cutthroat_competition, check_reality `Cutthroat_competition);
    ]
  in
  let actions_menu =
    let open MsgBox in
    make ~fonts ~x:202 ~y:8
    [
      make_entry "Call &Broker (F9)" @@ `Action `Action_call_broker;
      make_entry "&Survey (F10)" @@ `Action `Action_survey;
      make_entry "&Name RR" @@ `Action `Action_name_rr;
      make_entry "&Reality Levels" @@ `MsgBox reality_levels;
      make_entry "Re&tire" @@ `Action `Action_retire;
    ]
  in
  let titles =
    let open Menu.Title in
    [
      make ~fonts ~x:8 ~y:1 "&Game" game_menu;
      make ~fonts ~x:64 ~y:1 "D&isplays" displays_menu;
      make ~fonts ~x:120 ~y:1 "&Reports" reports_menu;
      make ~fonts ~x:176 ~y:1 "&Build" build_menu;
      make ~fonts ~x:242 ~y:1 "Ac&tions" actions_menu;
    ]
  in
  Menu.Global.make ~menu_h titles

let default win fonts =
  let screen = Utils.{
    w = R.width win;
    h = R.height win;
    x = 0;
    y = 0;
  }
  in
  let menu = Utils.{
    w = screen.w;
    h = 8;
    x = screen.x;
    y = screen.y;
  }
  in
  let mapview = Utils.{
    x = 0;
    y = menu.h;
    w = Gmap.map_width;
    h = Gmap.map_height;
  }
  in
  let ui = Utils.{
    x = Gmap.map_width - 1;
    y = menu.h + menu.y;
    w = 64;
    h = screen.h - menu.h;
  }
  in
  let minimap = Utils.{
    x=ui.x;
    y=ui.y;
    w=ui.w;
    h=55;
  }
  in
  let infobar = Utils.{
    x = ui.x;
    y = minimap.y + minimap.h;
    h = 19;
    w = ui.w;
  }
  in
  let train_ui = Utils.{
    x = ui.x;
    y = infobar.y + infobar.h;
    h = 117;
    w = ui.w;
  }
  in
  let dims =
    {
      screen;
      menu;
      mapview;
      ui;
      minimap;
      infobar;
      train_ui;
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
    menu = main_menu fonts dims.menu.h;
    view = Mapview.default dims.mapview;
    options;
    mode=Normal;
  }

let build_station_menu fonts =
  let open Menu in
  let open MsgBox in
  make ~fonts ~heading:"Type of facility?" ~x:202 ~y:16
  [
    make_entry "Si&gnal Tower ($25,000)" @@ `Action(Station.SignalTower);
    make_entry "&Depot ($50,000)" @@ `Action(Station.Depot);
    make_entry "&Station ($100,000)" @@ `Action(Station.Station);
    make_entry "&Terminal ($200,000)" @@ `Action(Station.Terminal);
  ]

let update (s:State.t) v (event:Event.t) =
  match v.mode with
  | Normal ->
    let v, menu_action, event =
      match Menu.Global.update s v.menu event with
      | _, Menu.NoAction ->
          (* Only update view if we have a change *)
          v, Menu.NoAction, event
      | menu, a ->
          (* Cancel out events we handled *)
          {v with menu}, a, NoEvent 
    in
    (* TODO: use menu_action *)
    let view, backend_actions =
      let dims = v.dims in
      Mapview.update s v.view event ~minimap:dims.minimap
    in
    let v =
      match menu_action with
      | On `Build_station ->
          let menu = build_station_menu s.textures.fonts in
          {v with mode=BuildStation(menu)}
      | _ -> v
    in
    v.view <- view;
    v, backend_actions

  | BuildStation build_menu ->
      (* Build Station mode *)
      let build_menu, action = Menu.MsgBox.update s build_menu event in
      (* TODO: build_menu *)
      match action with
      | Menu.NoAction ->
          (* Exit build station mode *)
          {v with mode=Normal}, []
      | Menu.On(station_kind) ->
          let x, y = Mapview.get_cursor_pos v.view in
          begin match Backend.check_build_station s.backend ~x ~y ~player:0 station_kind with
          | `Ok -> 
              let backend_action = [B.Action.BuildStation{x; y; kind=station_kind}] in
              {v with mode=Normal}, backend_action
              (* TODO: handle other cases *)
          | _ ->
              {v with mode=Normal}, []
          end
      | _ ->
          (* Update build menu *)
          {v with mode=BuildStation(build_menu)}, []


let render (win:R.window) (s:State.t) v =
  let dims = v.dims in
  (* Render main view *)
  let build_station = match v.mode with
    | Normal -> false
    | BuildStation _ -> true
  in
  
  let s = Mapview.render win s v.view ~minimap:dims.minimap ~build_station in

  (* Menu bar background *)
  R.draw_rect win ~x:0 ~y:0 ~w:dims.screen.w ~h:dims.menu.h ~color:Ega.cyan ~fill:true;
  let h = dims.screen.h - dims.menu.h in
  let y = dims.menu.h in

  (* Screen White border *)
  R.draw_rect win ~x:0 ~y ~w:dims.screen.w ~h ~color:Ega.white ~fill:false;

  let x = dims.ui.x in

  (* Border of UI *)
  R.draw_rect win ~x ~y ~h ~w:(dims.ui.w+1) ~color:Ega.white ~fill:false;

  (* Draw logo *)
  begin match Mapview.get_zoom v.view with
  | Zoom1 ->
      R.Texture.render ~x:(x+1) ~y:(y+1) win s.State.textures.Textures.logo;
  | _ -> ()
  end;

  (* Info bar *)
  let y = y + dims.minimap.h in
  R.draw_rect win ~x ~y ~h:dims.infobar.h ~w:dims.ui.w ~color:Ega.white ~fill:true;

  (* Train area *)
  let y = y + dims.infobar.h in
  R.draw_rect win ~x:(x+1) ~y:y ~h:dims.train_ui.h ~w:(dims.ui.w-1) ~color:Ega.bblue ~fill:true;

  (* Menu bar *)
  Menu.Global.render win s s.textures.fonts v.menu;

  s

