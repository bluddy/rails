open! Containers
open Utils.Infix
open Main_ui_d

let src = Logs.Src.create "main_ui" ~doc:"Main_ui"
module Log = (val Logs.src_log src: Logs.LOG)

module R = Renderer
module B = Backend
module C = Constants

let save_game (state:State.t) =
  let s1 = Backend.yojson_of_t state.backend |> Yojson.Safe.to_string in
  let s2 = Main_ui_d.yojson_of_options state.ui.options |> Yojson.Safe.to_string in
  let s3 = Mapview_d.yojson_of_t state.ui.view |> Yojson.Safe.to_string in
  let s = String.concat "====" [s1; s2; s3] in
  ignore(IO.File.write "./save.txt" s);
  print_endline "Saved Game"

(* Create menu *)
let main_menu fonts menu_h region =
  let open Menu in
  let game_speed =
    let check_speed speed (s:State.t) =
      B_options.equal_speed (Backend.get_speed s.backend) speed in
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
    let check_message message (s:State.t) = Main_ui_d.equal_message_speed s.ui.options.message_speed message in
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
      make_entry "&Efficiency" @@ `Action `Efficiency_report;
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
      make_entry "&Area Display (F2)" @@ `Action(`Display(Mapview_d.def_zoom2));
      make_entry "&Local Display (F3)" @@ `Action(`Display(Mapview_d.def_zoom3));
      make_entry "&Detail Display (F4)" @@ `Action(`Display(Mapview_d.Zoom4));
      make_entry "&Options" @@ `MsgBox options;
      make_entry "&Find City" @@ `Action `Find_city;
    ]
  in
  let is_zoom4 (s:State.t) = Mapview.is_zoom4 s.ui.view in
  let is_station4 (s:State.t) =
    Mapview.is_zoom4 s.ui.view && Mapview.const_box_on_station s.backend s.ui.view in
  let is_woodbridge4 (s:State.t) =
    Mapview.is_zoom4 s.ui.view && Mapview.const_box_on_woodbridge s.backend s.ui.view in
  let improve_station =
    let check_upgrade ?(flip=false) upgrade (s:State.t) =
      let station = Mapview.get_station_under_cursor_exn s.backend s.ui.view in
      let is_mem = Station.Upgrades.mem (Station.get_upgrades station) upgrade in
      if flip then not is_mem else is_mem
    in
    let open MsgBox in
    let module S = Station in
    let entry str upgrade =
      let price_s =
        let cash = Station.price_of_upgrade upgrade in
        Printf.sprintf " (%s)" (Utils.show_cash ~region cash)
      in
      make_entry (str^price_s) ~test_enabled:(check_upgrade ~flip:true upgrade)
        (`Checkbox(`ImproveStation upgrade, check_upgrade upgrade))
    in
    make ~fonts ~heading:"Improve Station"
    [
      entry "&Engine Shop" S.EngineShop;
      entry "&Switching Yard" S.SwitchingYard;
      entry "&Maintenance Shop" S.MaintenanceShop;
      entry "&Cold Storage" S.ColdStorage;
      entry "&LivestockPens" S.LivestockPens;
      entry "&Goods Storage" S.GoodsStorage;
      entry "&Post Office" S.PostOffice;
      entry "&Restaurant" S.Restaurant;
      entry "&Hotel" S.Hotel;
    ]
  in
  let build_menu =
    let check_trackbuild build (s:State.t) = Bool.equal s.ui.view.build_mode build in
    let open MsgBox in
    make ~fonts ~x:168 ~y:8
    [
      make_entry "New &Train (F7)" @@ `Action `Build_train;
      make_entry "Build &Station (F8)" ~test_enabled:is_zoom4 @@ `Action `Build_station;
      make_entry "Build &Industry" ~test_enabled:is_zoom4 @@ `Action `Build_industry;
      make_entry "&Build Track" ~test_enabled:is_zoom4 @@ `Checkbox(`BuildTrack, check_trackbuild true); 
      make_entry "&Remove Track" ~test_enabled:is_zoom4 @@ `Checkbox(`RemoveTrack, check_trackbuild false);
      make_entry "Im&prove Station" ~test_enabled:is_station4 @@ `MsgBox improve_station;
      make_entry "Up&grade Bridge" ~test_enabled:is_woodbridge4 @@ `Action `Upgrade_bridge;
    ]
  in
  let reality_levels =
    let check_reality level (s:State.t) =
      B_options.RealityLevels.mem s.backend.options.reality_levels level
    in
    let open MsgBox in
    make ~fonts ~x:0 ~y:0
    [
      make_entry "&Dispatcher Operations" @@
        `Checkbox(`Reality_level `DispatcherOps, check_reality `DispatcherOps);
      make_entry "Complex &Economy" @@
        `Checkbox(`Reality_level `ComplexEconomy, check_reality `ComplexEconomy);
      make_entry "&Cut-Throat Competition" @@
        `Checkbox(`Reality_level `CutthroatCompetition, check_reality `CutthroatCompetition);
    ]
  in
  let actions_menu =
    let open MsgBox in
    make ~fonts ~x:202 ~y:8
    [
      make_entry "Call &Broker (F9)" @@ `Action `Call_broker;
      make_entry "&Survey (F10)" @@ `Checkbox(`Survey, fun (s:State.t) -> Mapview.get_survey s.ui.view);
      make_entry "&Name RR" @@ `Action `Name_rr;
      make_entry "&Reality Levels" @@ `MsgBox reality_levels;
      make_entry "Re&tire" @@ `Action `Retire;
    ]
  in
  let titles =
    let open Menu.Title in
    [
      make ~fonts ~x:8 ~y:1 "&Game" game_menu;
      make ~fonts ~x:64 ~y:1 "D&isplays" displays_menu;
      make ~fonts ~x:120 ~y:1 "&Reports" reports_menu;
      make ~fonts ~x:210 ~y:1 "&Cheats" (Cheats.make_menu fonts);
      make ~fonts ~x:176 ~y:1 "&Build" build_menu;
      make ~fonts ~x:252 ~y:1 "Ac&tions" actions_menu; (* was x:242 *)
    ]
  in
  Menu.Global.make ~menu_h titles

let default ?options ?view win fonts region =
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
    w = Tilemap.map_width_default;
    h = Tilemap.map_height_default;
  }
  in
  let ui = Utils.{
    x = Tilemap.map_width_default - 1;
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
      train_ui_train_h=5;
    }
  in
  let options = match options with
    | Some options -> options
    | None ->
      {
        message_speed=`Slow;
        news=NewsTypes.of_list [`Financial; `Railroad; `Local];
        features=Features.of_list [`Animations; `Sounds];
      }
  in
  let view = match view with
    | Some view -> view
    | None -> Mapview.default dims.mapview
  in
  let menu = main_menu fonts dims.menu.h region in
  {
    dims;
    menu;
    view;
    options;
    train_ui_start=0;
    mode=Normal;
    train_arrival_msgs = [];
  }

let build_station_menu fonts region =
  let open Menu in
  let open MsgBox in
  let open Printf in
  let price station = Station.price_of station |> Utils.show_cash ~region in
  make ~fonts ~heading:"Type of facility?" ~x:176 ~y:16
  [
    make_entry "&CANCEL" @@ `Action(None);
    make_entry (sprintf "Si&gnal Tower (%s)" @@ price `SignalTower) @@ `Action(Some `SignalTower);
    make_entry (sprintf "&Depot (%s)" @@ price `Depot) @@ `Action(Some `Depot);
    make_entry (sprintf "&Station (%s)" @@ price `Station) @@ `Action(Some `Station);
    make_entry (sprintf "&Terminal (%s)" @@ price `Terminal) @@ `Action(Some `Terminal);
  ]

let build_signal_menu fonts x y =
  let open Menu in
  let open MsgBox in
  make ~fonts ~x ~y
  [
    make_entry "NORMAL" @@ `Action(Some `Normal);
    make_entry "HOLD" @@ `Action(Some `Hold);
    make_entry "PROCEED" @@ `Action(Some `Proceed);
  ]

let build_bridge_menu fonts region =
  let open Menu in
  let open MsgBox in
  let open Bridge in
  let open Printf in
  let price bridge =
    Bridge.price_of bridge
    |> Utils.show_cash ~region 
  in
  make ~fonts ~heading:"Type of bridge?" ~x:176 ~y:16
  [
    make_entry "&CANCEL" @@ `Action(None);
    make_entry (sprintf "&Wooden Trestle (%s)" @@ price Wood) @@ `Action(Some Wood);
    make_entry (sprintf "&Stone Masonry (%s)" @@ price Stone) @@ `Action(Some Stone);
    make_entry (sprintf "&Iron Girder (%s)" @@ price Iron) @@ `Action(Some Iron);
  ]

let build_high_grade_menu fonts ~grade ~tunnel =
  let open Menu in
  let open MsgBox in
  let pct1 = grade / 8 in
  let pct2 = ((grade / 2) mod 4) * 25 in
  let heading = Printf.sprintf "WARNING: %d.%d%% grade" pct1 pct2 in
  let entries =
  [
    make_entry "&Build Track" @@ `Action(Some `BuildTrack);
    make_entry "&CANCEL" @@ `Action(None);
  ]
  in
  let entries =
    if tunnel then entries @ [make_entry "Build &Tunnel" @@ `Action(Some `BuildTunnel)]
    else entries
  in
  make ~fonts ~heading ~x:176 ~y:16 entries

let build_tunnel_menu fonts ~length ~cost ~region =
  let open Menu in
  let open MsgBox in
  let heading =
    Printf.sprintf "%d mile tunnel required.\nCost: %s"
      length
      (Utils.show_cash ~region cost)
  in
  let entries =
  [
    make_entry "&Build Tunnel" @@ `Action(Some true);
    make_entry "&Never Mind" @@ `Action None;
  ]
  in
  make ~fonts ~heading ~x:176 ~y:50 entries

  (* Iterate over trains in train roster *)
let train_roster_iter (s:State.t) v f =
  let train_h = v.dims.train_ui_train_h in
  let max_fit_trains = v.dims.train_ui.h / train_h in
  let max_draw_trains = min max_fit_trains @@
    (Trainmap.size s.backend.players.(0).trains) - v.train_ui_start
  in
  Iter.iter (fun i ->
    f (v.dims.train_ui.y + 1 + (i + 1) * train_h) (v.train_ui_start + i)
  )
  Iter.(0 -- (max_draw_trains - 1))

let handle_train_roster_event (s:State.t) v event =
  let ui_train_find f =
    let train_h = v.dims.train_ui_train_h in
    let max_fit_trains = v.dims.train_ui.h / train_h in
    let max_draw_trains = min max_fit_trains @@
      (Trainmap.size s.backend.players.(0).trains) - v.train_ui_start
    in
    Iter.find (fun i ->
      f (v.dims.train_ui.y + 1 + (i + 1) * train_h) (v.train_ui_start + i)
    )
    Iter.(0 -- (max_draw_trains - 1))
  in
  match event with
  | Event.MouseButton {x; y; button=`Left; down=true; _} when
      x > v.dims.train_ui.x && y > v.dims.train_ui.y ->
        let res =
          ui_train_find (fun y_bot train_idx ->
            if y < y_bot then Some (v, `EditTrain (Trainmap.Id.of_int train_idx))
            else None)
        in
        Option.get_or ~default:(v, `NoAction) res
  | _ -> v, `NoAction

let nobaction = B.Action.NoAction

let make_msgbox ?x ?y s v ~fonts text =
  let menu = Menu.MsgBox.make_basic ?x ?y s ~fonts text in
  let mode = ModalMsgbox {menu; data=(); last=Normal} in
  {v with mode}, nobaction

let handle_event (s:State.t) v (event:Event.t) =
  (* Handle most stuff for regular menus and msgboxes
     process_fn: main processing on choice
     build_fn: rebuild: new state
   *)
  let handle_modal_menu_events =
      fun (type b) (type c)
          ?(is_msgbox=false)
          (modal: (State.t, b, c) modalmenu)
          (build_fn:(State.t, b, c) modalmenu -> State.t mode)
          (process_fn:(State.t, b, c) modalmenu -> b -> State.t t * B.Action.t) ->
      let menu, action = Menu.MsgBox.update s modal.menu event in
      let exit_mode () = {v with mode=modal.last}, B.Action.NoAction in
      begin match action with
      | Menu.On(None) -> exit_mode ()
      | Menu.NoAction when Event.pressed_esc event -> exit_mode ()
      | Menu.NoAction when is_msgbox && Event.key_modal_dismiss event -> exit_mode ()
      | Menu.ClickInMsgBox when is_msgbox -> exit_mode ()
      | Menu.On(Some choice) -> process_fn modal choice
      | Menu.NoAction -> v, B.Action.NoAction
      | _ -> {v with mode=build_fn {modal with menu}}, B.Action.NoAction
      end
  in
  let modal_screen_no_input v event =
    if Event.is_left_click event || Event.key_modal_dismiss event then
      {v with mode=Normal}, nobaction
    else
      v, nobaction
  in
  let old_mode, old_menu = v.mode, v.menu in
  let v, backend_msg =
    match v.mode with
    | Normal ->
      (* Main gameplay view *)
      let menu, menu_action, event = Menu.Global.update s v.menu event in
      let v = if menu =!= v.menu then {v with menu} else v in
      let view = match menu_action with
        | On(`Survey)  -> Mapview.set_survey v.view true
        | Off(`Survey) -> Mapview.set_survey v.view false
        | _ -> v.view
      in
      let v, view_action = handle_train_roster_event s v event in
      let view, view_action =
        match view_action with
        | `NoAction -> Mapview.handle_event s view event ~minimap:v.dims.minimap
        | _ -> view, view_action
      in
      if v.view =!= view then v.view <- view;

      let v, backend_action =
        match menu_action, view_action with
        | On `Build_train, _ ->
            {v with mode=BuildTrain(`ChooseEngine)}, nobaction
        | _, `EditTrain train_idx ->
            {v with mode=TrainReport(Train_report.make s train_idx)}, nobaction
        | On `Build_station, _ ->
            let menu =
              build_station_menu s.fonts s.backend.region
              |> Menu.MsgBox.do_open_menu s in
            let modal = {menu; data=(); last=Normal} in
            {v with mode=BuildStation modal}, nobaction
        | On `BuildTrack, _ ->
            {v with view=Mapview.set_build_mode v.view true}, nobaction 
        | On `RemoveTrack, _ ->
            {v with view=Mapview.set_build_mode v.view false}, nobaction 
        | On `ImproveStation upgrade, _ ->
            let x, y = Mapview.get_cursor_pos v.view in
            {v with mode=StationReport(x, y)}, ImproveStation{x; y; player=0; upgrade}
        | On `Save_game, _ ->
            save_game s;
            v, nobaction
        | On (`Speed speed), _ -> v, B.Action.SetSpeed speed
        | On (`Message setting), _ ->
            let options = {v.options with message_speed=setting} in
            {v with options}, nobaction
        | On (`News newstype), _ ->
            let options = v.options in
            let news = NewsTypes.add options.news newstype in
            let options = {options with news} in
            {v with options}, nobaction
        | Off (`News newstype), _ ->
            let options = v.options in
            let news = NewsTypes.remove options.news newstype in
            let options = {options with news} in
            {v with options}, nobaction
        | On (`Features feature), _ ->
            let options = v.options in
            let features = Features.add options.features feature in
            let options = {options with features} in
            {v with options}, nobaction
        | Off (`Features feature), _ ->
            let options = v.options in
            let features = Features.remove options.features feature in
            let options = {options with features} in
            {v with options}, nobaction
        | On (`Options option), _ ->
            {v with view=Mapview.update_option v.view option true}, nobaction
        | On (`Balance_sheet), _ ->
            let b = Balance_sheet.create s ~player_idx:0 in
            {v with mode=Balance_sheet b}, nobaction
        | On (`Income_statement), _ ->
            let b = Balance_sheet.create s ~player_idx:0 in
            {v with mode=Income_statement b}, nobaction
        | On (`Accomplishments), _ ->
            {v with mode=Accomplishments}, nobaction
        | On (`Efficiency_report), _ ->
            {v with mode=Efficiency_report}, nobaction
        | On (`Call_broker), _ ->
            v, B.Action.CallBroker{player=C.player}
        | On (`Cheat x), _ ->
            v, B.Action.Cheat(C.player, x)
        | Off (`Options option), _ ->
            {v with view=Mapview.update_option v.view option false}, nobaction
        | _, `BuildTrack msg  -> v, B.Action.BuildTrack msg
        | _, `RemoveTrack msg -> v, B.Action.RemoveTrack msg
        | _, `BuildFerry msg  -> v, B.Action.BuildFerry msg
        | _, `BuildBridge msg ->
            let menu = build_bridge_menu s.fonts s.backend.region
              |> Menu.MsgBox.do_open_menu s in
            let modal = {menu; data=msg; last=Normal} in
            {v with mode=BuildBridge modal}, nobaction
        | _, `HighGradeTrack(msg, grade, tunnel) ->
            let menu = build_high_grade_menu ~grade ~tunnel s.fonts
              |> Menu.MsgBox.do_open_menu ~selected:(Some 0) s
            in
            let modal = {menu; data=msg; last=Normal} in
            {v with mode=BuildHighGrade modal}, nobaction
        | _, `ShowTileInfo (x, y, tile) ->
            let info = Tile.Info.get (B.get_region s.backend) tile in
            let open Menu.MsgBox in
            let entries =
              let tilename = match tile with
              | City | Village ->
                  begin match B.find_close_city s.backend x y ~range:4 with
                  | Some (x,y) ->
                      let city, _ = Cities.find_exn s.backend.cities x y in
                      Printf.sprintf "%s (%s)" (Tile.show tile) city
                  | None -> Tile.show tile
                  end
              | _ -> Tile.show tile
              in
              let entries =
              [
                static_entry ~color:Ega.white tilename;
                static_entry ~color:Ega.white "Right-of-Way costs";
                static_entry ~color:Ega.white @@
                  Printf.sprintf "%s per mile" (Utils.show_cash ~region:s.backend.region info.cost);
              ]
              in
              let demand = match info.demand with
                | [] -> []
                | demand ->
                    static_entry ~color:Ega.bcyan " Demands" ::
                    List.map (fun (good, amount) ->
                      let prefix = match amount with
                        | 8  -> " 1/8 "
                        | 16 -> " 1/4 "
                        | 32 -> " 1/2 "
                        | _ -> " "
                      in
                      static_entry ~color:Ega.black @@ prefix ^ Goods.show good
                    )
                    demand
              in
              let supply = match info.supply with
                | [] -> []
                | supply ->
                    static_entry ~color:Ega.bcyan " Supplies" ::
                    List.map (fun (good, _) ->
                      static_entry ~color:Ega.black @@ " "^Goods.show good)
                      supply
              in
              let convert =
                List.filter_map (fun (good, _) ->
                  match Goods.convert s.backend.region good with
                  | None -> None
                  | Some good ->
                    static_entry ~color:Ega.black @@ " ("^Goods.show good^")"
                    |> Option.some
                )
                info.demand
              in
              entries @ demand @ supply @ convert
            in
            let menu =
              Menu.MsgBox.make ~x:100 ~y:50 ~fonts:s.fonts entries ~font_idx:4
              |> Menu.MsgBox.do_open_menu s
            in
            let mode = ModalMsgbox {menu; data=(); last=Normal} in
            {v with mode}, nobaction

        | _, `SignalMenu(x, y, dir, screen_x, screen_y) ->
            let menu =
              build_signal_menu s.fonts screen_x screen_y
              (* build_signal_menu s.fonts 10 10 (*((x + 1) * C.tile_w) (y * C.tile_h + v.dims.menu.y) *) *)
              |> Menu.MsgBox.do_open_menu s
            in
            let modal = {menu; data=(x, y, dir); last=Normal} in
            {v with mode=SignalMenu modal}, nobaction

        | _, `StationView(x, y) ->
            {v with mode=StationReport(x, y)}, nobaction

        | _, `DoubleTrack(double, x, y, player) ->
            v, B.Action.DoubleTrack{x; y; double; player}

        | _ ->
            v, nobaction
      in
      v, backend_action

    | ModalMsgbox menu ->
        handle_modal_menu_events ~is_msgbox:true menu
        (fun x -> ModalMsgbox x)
        (fun _ () -> v, nobaction)

    | BuildStation build_menu ->
        handle_modal_menu_events build_menu
        (fun x -> BuildStation x)
        (fun modal station_kind ->
            let exit_mode () = {v with mode=modal.last}, nobaction in
            let x, y = Mapview.get_cursor_pos v.view in
            match Backend.check_build_station s.backend ~x ~y ~player:0 station_kind with
            | `Ok ->
                let backend_action = B.Action.BuildStation{x; y; kind=station_kind; player=0} in
                {v with mode=modal.last}, backend_action
                (* TODO: handle other cases *)
            | _ -> exit_mode ()
            )

    | BuildBridge build_menu ->
        handle_modal_menu_events build_menu
        (fun x -> BuildBridge x)
        (fun modal bridge_kind ->
            let msg = modal.data in
            let x, y, dir, player = msg.x, msg.y, msg.dir, msg.player in
            match Backend.check_build_bridge s.backend ~x ~y ~dir ~player with
            | `Ok -> 
                let backend_action = B.Action.BuildBridge(modal.data, bridge_kind) in
                let view = Mapview.move_const_box v.view dir 2 in
                {v with mode=modal.last; view}, backend_action
            | _ ->
                {v with mode=modal.last}, nobaction
            )

    | BuildHighGrade build_menu ->
        let fonts = s.fonts in
        handle_modal_menu_events
        build_menu
        (fun x -> BuildHighGrade x)
        (fun ({data={x;y;dir;player} as msg;_} as modal) -> function
          | `BuildTrack ->
              let view = Mapview.move_const_box v.view dir 1 in
              {v with mode=modal.last; view}, B.Action.BuildTrack msg
          | `BuildTunnel ->
              match B.check_build_tunnel s.backend ~x ~y ~player ~dir with
              | `Tunnel(length, disp_length, cost) ->
                  let menu =
                    build_tunnel_menu ~length:disp_length ~cost
                      ~region:s.backend.region s.fonts
                    |> Menu.MsgBox.do_open_menu ~selected:(Some 0) s
                  in
                  let modal = {menu; data=(msg, length); last=Normal} in
                  {v with mode=BuildTunnel modal}, nobaction
              | `HitWater ->
                  make_msgbox s v ~fonts "Can't tunnel under water!"
              | `HitsTrack ->
                  make_msgbox s v ~fonts "Tunnel can't cross\nexisting track!"
              | `OutOfBounds ->
                  make_msgbox s v ~fonts "Can't tunnel off map."
              | `TooLong ->
                  make_msgbox s v ~fonts "Tunnel too long."
        )

    | BuildTunnel build_menu ->
      handle_modal_menu_events
      build_menu
        (fun x -> BuildTunnel x)
        (fun {data=(msg, length);_} -> function
          | true ->
              let view = Mapview.move_const_box v.view msg.dir length in
              {v with mode=Normal; view}, B.Action.BuildTunnel(msg, length)
          | _ -> {v with mode=Normal}, nobaction
        )

    | SignalMenu signal_menu ->
      handle_modal_menu_events
      signal_menu
        (fun x -> SignalMenu x)
        (fun {data=(x, y, dir);_} cmd ->
          {v with mode=Normal}, StationSetSignal{x; y; dir; cmd})

    | BuildTrain state ->
        let state2, action = Build_train.handle_event s state event in
        let v = 
          if state2 =!= state then {v with mode=BuildTrain(state2)} else v
        in
        v, action

    | TrainReport state ->
        let exit_state, state2, action = Train_report.handle_event s state event in
        let v =
          if exit_state then {v with mode=Normal}
          else if state =!= state2 then {v with mode=TrainReport state2}
          else v
        in
        v, action

    | Stock_broker state ->
       let exit_state, state2, action = Stock_broker.handle_event s state event in
        let v =
          if exit_state then {v with mode=Normal}
          else if state =!= state2 then {v with mode=Stock_broker state2}
          else v
        in
        v, action

    | StationReport _
    | Balance_sheet _
    | Income_statement _
    | Efficiency_report
    | Accomplishments -> modal_screen_no_input v event
        
  in
  let check_pause old _new = match old,_new with
    | true, false -> `Pause
    | false, true -> `Unpause
    | _ -> `DoNothing
  in
  let mode_pause = check_pause (is_normal_mode old_mode) (is_normal_mode v.mode) in
  let menu_pause = check_pause (Menu.Global.is_closed old_menu) (Menu.Global.is_closed v.menu) in
  let backend_msgs = match mode_pause, menu_pause with
   | `Pause, _ | _, `Pause -> [backend_msg; B.Action.Pause]
   | `Unpause, _ | _, `Unpause -> [backend_msg; B.Action.Unpause]
   | _ -> [backend_msg]
  in
  v, backend_msgs

  

(* Handle incoming messages from backend *)
let handle_msgs (s:State.t) v ui_msgs =
  let train_arrival_msg_speed v : [`Fast | `Slow] option =
    match v.options.message_speed with
    | `Off -> None
    (* Turbo mode cancels train messges *)
    | (`Fast | `Slow) as x when not (B_options.equal_speed s.backend.options.speed `Turbo) -> Some x
    | _ -> None
  in
  let handle_msg v ui_msg =
    match v.mode, ui_msg with

    | BuildTrain(`AddCars _), Backend.TrainBuilt idx ->
        let state = Train_report.make s idx in
        {v with mode=TrainReport state}

    | Stock_broker state, _ ->
        let state2 = Stock_broker.handle_msg s state ui_msg in
        if state2 === state then v else {v with mode=Stock_broker state2}

    | Normal, (TrainArrival t) ->
        let msg_speed = train_arrival_msg_speed v in
        Option.map_or ~default:v
          (fun msg_speed ->
             let time = match msg_speed with
             | `Fast -> C.fast_message_time
             | `Slow -> C.slow_message_time
             in
             Log.debug (fun f -> f "Setting train arrival message with %d time" time);
             {v with train_arrival_msgs=v.train_arrival_msgs @ [t, ref time] } (* TODO: get proper timer *)
          )
          msg_speed

    | Normal, OpenStockBroker{player} when player = C.player ->
      let state = Stock_broker.make s in
      {v with mode=Stock_broker state}

    (* TODO: handle demand changed msg *)
    | _ -> v
  in
  List.fold_left handle_msg v ui_msgs

  (* Mostly animations. *)
let handle_tick s v time is_cycle = match v.mode with
  | BuildTrain(`AddCars state) ->
      let state2 = Build_train.AddCars.handle_tick s state time in
      if state === state2 then v else {v with mode=BuildTrain(`AddCars state2)}
  | TrainReport state ->
      let state2 = Train_report.handle_tick state time in
      if state === state2 then v else {v with mode=TrainReport state2}
  | Normal ->
      let view = Mapview.handle_tick s v.view time is_cycle in
      let decr_train_msgs () =
        let train_arrival_msgs = match v.train_arrival_msgs with
        | ((_, t)::_) as msgs when !t > 0 ->
            decr t;
            msgs
        | _::msgs -> msgs
        | [] -> []
        in
        [%up {v with view; train_arrival_msgs}]
      in
      decr_train_msgs ()
  | _ -> v

let str_of_month = [|"Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec"|]

let draw_train_roster win (s:State.t) v =
  train_roster_iter s v
  (fun y_bot idx ->
    let train = Trainmap.get s.backend.players.(0).trains (Trainmap.Id.of_int idx) in
    (* Speed line *)
    let x = v.dims.train_ui.x + 1 in
    let x2 = v.dims.train_ui.x + v.dims.train_ui.w - 1 in
    let y = y_bot in
    R.draw_line win ~x1:x ~y1:y ~x2 ~y2:y ~color:Ega.dgray;
    let x1 = x2 - (Train.get_speed train) * 2 in
    R.draw_line win ~x1 ~y1:y ~x2 ~y2:y ~color:Ega.bgreen;

    (* Draw UI train *)
    (* draw engine *)
    (* TODO: priority on board -> green *)
    let color = Ega.black in
    R.draw_line win ~x1:(x+3) ~y1:(y-3) ~x2:(x+3) ~y2:y ~color;
    R.draw_line win ~x1:(x+4) ~y1:(y-2) ~x2:(x+4) ~y2:(y-1) ~color;
    R.draw_rect win ~x:(x+5) ~y:(y-3) ~w:2 ~h:4 ~color ~fill:true;
    (* draw cars *)
    let _ =
      List.fold_left (fun x car ->
        let full = Train.Car.get_amount car > C.car_amount / 2 in
        let freight = Train.Car.get_good car in
        Ui_common.draw_ui_car win ~x ~y:(y-2) ~full freight;
        x + 5)
      (x + 8)
      train.cars
    in
    (* Draw destination *)
    let loc = Train.get_dest train in
    let station = Station_map.get_exn loc s.backend.stations in
    let short_name = Station.get_short_name station in
    Fonts.Render.write win s.fonts ~color:Ega.white ~idx:3
      short_name ~x:(x2-11) ~y:(y-4);
  )

let render_main win (s:State.t) v =
  let dims = v.dims in
  (* Render main view *)
  let build_station = match v.mode with
    | BuildStation _ -> true
    | _ -> false
  in
  
  let s = Mapview.render win s v.view ~minimap:dims.minimap ~build_station in

  (* Menu bar background *)
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
      let logo = Hashtbl.find s.State.textures.misc `Logo in
      R.Texture.render ~x:(x+1) ~y:(y+1) win logo;
  | _ -> ()
  end;

  let draw_train_arrival_msg (msg:Backend_d.train_arrival_msg) =
    let msg_s: string =
      let b = Buffer.create 100 in 
      let buf_add = Buffer.add_string b in
      buf_add " ...";
      buf_add @@ Backend.get_time_of_day msg.time;
      buf_add "...\n";
      begin match msg.train_name with
      | Some name ->
          buf_add name
      | None ->
          buf_add @@ Freight.show_complex msg.freight
      end;
      buf_add "\n";
      buf_add @@ Train.show_train_type msg._type;
      buf_add "  (";
      buf_add @@ string_of_int (msg.train_num + 1);
      buf_add ")\n";
      List.iter (fun (good, amount) ->
         buf_add @@ Goods.short_descr_of good amount)
        msg.goods_amount;
      buf_add "\nRev: ";
      buf_add @@ Utils.show_cash msg.revenue;
      Buffer.contents b
    in
    let x, y = (dims.minimap.x+1), (dims.minimap.y+1) in
    let h, w = dims.minimap.h - 1, dims.minimap.w - 1 in
    R.draw_rect win ~x ~y ~h ~w ~color:Ega.bblue ~fill:true;
    Fonts.Render.write win s.fonts ~color:Ega.white ~idx:3 ~x:258 ~y:12 msg_s
  in
  begin match v.train_arrival_msgs with
  | (msg, _)::_ -> draw_train_arrival_msg msg
  | _ -> ()
  end;

  (* Info bar *)
  let y = y + dims.minimap.h in
  R.draw_rect win ~x ~y ~h:dims.infobar.h ~w:dims.ui.w ~color:Ega.white ~fill:true;

  if Backend.broker_timer_active s.backend C.player then
    Fonts.Render.write win s.fonts ~color:Ega.bgreen ~idx:4 ~x:256 ~y:66 "B";

  let cash = B.get_cash s.backend ~player:0 in
  let cash_s = Utils.show_cash ~spaces:6 ~region:s.backend.region cash in
  Fonts.Render.write win s.fonts ~color:Ega.black ~idx:4 ~x:264 ~y:66 cash_s;

  let month, year = B.get_date s.backend in
  let date_s = Printf.sprintf "%s %d" (str_of_month.(month)) year in
  Fonts.Render.write win s.fonts ~color:Ega.black ~idx:4 ~x:264 ~y:74 date_s;

  (* Train area *)
  let y = y + dims.infobar.h in
  R.draw_rect win ~x:(x+1) ~y:y ~h:dims.train_ui.h ~w:(dims.ui.w-1) ~color:Ega.bblue ~fill:true;
  draw_train_roster win s v;

  (* Menu bar *)
  Menu.Global.render win s s.fonts v.menu ~w:dims.screen.w ~h:dims.menu.h;
  ()

let render (win:R.window) (s:State.t) v =
  (* Msgboxes *)
  let rec render_mode = function
    | Normal ->
        render_main win s v
    | ModalMsgbox modal ->
        render_mode modal.last;
        Menu.MsgBox.render win s modal.menu
    | BuildStation modal ->
        render_main win s v;
        Menu.MsgBox.render win s modal.menu
    | BuildBridge modal ->
        render_main win s v;
        Menu.MsgBox.render win s modal.menu
    | BuildHighGrade modal ->
        render_main win s v;
        Menu.MsgBox.render win s modal.menu
    | BuildTunnel modal ->
        render_main win s v;
        Menu.MsgBox.render win s modal.menu
    | SignalMenu modal ->
        render_main win s v;
        Menu.MsgBox.render win s modal.menu
    | StationReport(x, y) ->
        Station_report.render win s (x,y) ~show_demand:true
    | BuildTrain(state) ->
        Build_train.render win s state
    | TrainReport state ->
        Train_report.render win s state
    | Stock_broker state ->
        Stock_broker.render win s state
    | Balance_sheet state ->
        Balance_sheet_view.render win s state
    | Income_statement state ->
        Income_statement_view.render win s state
    | Accomplishments ->
        Accomplishments.render win s
    | Efficiency_report ->
        Efficiency_report.render win s
  in
  render_mode v.mode

