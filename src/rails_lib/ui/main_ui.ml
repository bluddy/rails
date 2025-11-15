open! Containers
open Utils.Infix
open Main_ui_d

let src = Logs.Src.create "main_ui" ~doc:"Main_ui"
module Log = (val Logs.src_log src: Logs.LOG)

module R = Renderer
module B = Backend
module C = Constants
module M = Money

let sp = Printf.sprintf

let save_game (state:State.t) =
  let s1 = Backend.yojson_of_t state.backend |> Yojson.Safe.to_string in
  let s2 = Main_ui_d.yojson_of_options state.ui.options |> Yojson.Safe.to_string in
  let s3 = Mapview_d.yojson_of_t state.ui.view |> Yojson.Safe.to_string in
  let s = String.concat "====" [s1; s2; s3] in
  ignore(IO.File.write "./save.txt" s);
  print_endline "Saved Game"

(* Create menu *)
let main_menu fonts ~h ~w region =
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
      make_entry "Quit" @@ `Action `Quit_game;
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
        sp " (%s)" (M.print ~region cash)
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
      B_options.RealityLevels.mem (B.get_options s.backend).reality_levels level
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
      make ~fonts ~x:210 ~y:1 "Cheats" (Cheats.make_menu fonts);
      make ~fonts ~x:176 ~y:1 "&Build" build_menu;
      make ~fonts ~x:252 ~y:1 "Ac&tions" actions_menu; (* was x:242 *)
    ]
  in
  Menu.Animated.make_global ~h ~w fonts titles

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
  let menu = main_menu fonts ~w:dims.menu.w ~h:dims.menu.h region in
  {
    dims;
    menu;
    view;
    options;
    train_ui_start=0;
    mode=Normal;
    next_modes = [];
    train_arrival_msgs = [];
  }

let set_modes l v = match l with
  | x::xs -> {v with mode=x; next_modes=xs}
  | [] -> v

let next_mode s v = match v.next_modes with
  | (GenericScreen{start_fn; _} as x)::xs ->
      start_fn s;
      {v with mode=x; next_modes=xs}
  | (Income_statement{start_fn; _} as x)::xs ->
      start_fn s;
      {v with mode=x; next_modes=xs}
  | x::xs ->
      {v with mode=x; next_modes=xs}
  | [] ->
      Sound.stop_music ();
      [%up {v with mode=Normal}]

let build_station_menu fonts region =
  let open Menu in
  let open MsgBox in
  let open Printf in
  let price station = Station.price_of station |> M.print ~region in
  make ~fonts ~heading:"Type of facility?" ~x:176 ~y:16
  [
    make_entry "&CANCEL" @@ `Action(None);
    make_entry (sprintf "Si&gnal Tower (%s)" @@ price `SignalTower) @@ `Action(Some `SignalTower);
    make_entry (sprintf "&Depot (%s)" @@ price `Depot) @@ `Action(Some `Depot);
    make_entry (sprintf "&Station (%s)" @@ price `Station) @@ `Action(Some `Station);
    make_entry (sprintf "&Terminal (%s)" @@ price `Terminal) @@ `Action(Some `Terminal);
  ]

let build_industry_menu fonts region =
  let open Menu in
  let open MsgBox in
  let open Printf in
  let entries = Tile.Info.map_industry region @@
    fun (tile, info) ->
      make_entry
        (sprintf "%s  %s" (Tile.show tile) M.(print ~region @@ info.cost * C.build_industry_mult)) @@
        `Action(Some tile)
  in
  make ~fonts ~heading:"Build..." ~x:176 ~y:16 @@
    (make_entry "NONE" @@ `Action(None)) ::
    entries

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
  let price bridge = Bridge.price_of bridge |> M.print ~region in
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
  let heading = sp "WARNING: %d.%d%% grade" pct1 pct2 in
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
    sp "%d mile tunnel required.\nCost: %s" length (M.print ~region cost)
  in
  let entries =
  [
    make_entry "&Build Tunnel" @@ `Action(Some true);
    make_entry "&Never Mind" @@ `Action None;
  ]
  in
  make ~fonts ~heading ~x:176 ~y:50 entries

let confirm_build_site_menu fonts =
  let open Menu in
  let open MsgBox in
  make ~fonts ~heading:"Site selected..." ~x:144 ~y:24 [
    make_entry "Build" @@ `Action(Some ());
    make_entry "Cancel" @@ `Action None;
  ]

  (* Iterate over trains in train roster *)
let train_roster_iter (s:State.t) v f =
  let train_h = v.dims.train_ui_train_h in
  let max_fit_trains = v.dims.train_ui.h / train_h in
  let player_idx = C.player in
  let max_draw_trains = min max_fit_trains @@
    (Trainmap.size @@ B.get_trains player_idx s.backend) - v.train_ui_start
  in
  Iter.iter (fun i ->
    f (v.dims.train_ui.y + 1 + (i + 1) * train_h) (v.train_ui_start + i)
  )
  Iter.(0 -- (max_draw_trains - 1))

let handle_train_roster_event (s:State.t) v event =
  let ui_train_find f =
    let train_h = v.dims.train_ui_train_h in
    let max_fit_trains = v.dims.train_ui.h / train_h in
    let player_idx = C.player in
    let max_draw_trains = min max_fit_trains @@
      (Trainmap.size @@ B.get_trains player_idx s.backend) - v.train_ui_start
    in
    Iter.find (fun i ->
      f (v.dims.train_ui.y + 1 + (i + 1) * train_h) (v.train_ui_start + i)
    )
    Iter.(0 -- (max_draw_trains - 1))
  in
  match event with
  | Event.MouseButton {x; y; button; down=true; _} when
      x > v.dims.train_ui.x && y > v.dims.train_ui.y ->
        let train_idx =
          ui_train_find (fun y_bot train_idx ->
            if y < y_bot then Some (Trainmap.Id.of_int train_idx)
            else None)
        in
        let action = match train_idx, button with
          | Some idx, `Left -> `EditTrain idx
          | Some idx, `Right -> `HoldTrainToggle idx
          | _ -> `NoAction
        in
        v, action

  | _ -> v, `NoAction

let nobaction = B.Action.NoAction

let make_msgbox_mode ?x ?y ?background s text =
  if String.length text = 0 then Normal else
  let menu = Menu.MsgBox.make_basic ?x ?y s ~fonts:s.State.fonts text in
  let modal = make_modal ?background menu () in
  ModalMsgbox modal

let make_msgbox ?x ?y s v text =
  let mode = make_msgbox_mode ?x ?y s text in
  {v with mode}, nobaction

let check_add_pause_msg old_mode old_menu v =
  let check_pause old _new = match old,_new with
    | true, false -> `Pause
    | false, true -> `Unpause
    | _ -> `DoNothing
  in
  let mode_pause = check_pause (is_normal_mode old_mode) (is_normal_mode v.mode) in
  let menu_pause = check_pause (Menu.Animated.is_closed old_menu) (Menu.Animated.is_closed v.menu) in
  match mode_pause, menu_pause with
   | `Pause, _ | _, `Pause -> [B.Action.Pause]
   | `Unpause, _ | _, `Unpause -> [B.Action.Unpause]
   | _ -> []

let modal_screen_no_input s v event =
  let v =
    if Event.modal_dismiss event then next_mode s v
    else v in
  v, nobaction

let _build_station_mode (s:State.t) v =
  let menu =
    build_station_menu s.fonts (B.get_region s.backend)
    |> Menu.MsgBox.do_open_menu s in
  let modal = make_modal menu () in
  {v with mode=BuildStation modal}

let handle_event (s:State.t) v (event:Event.t) time =
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
      let menu, action = Menu.MsgBox.handle_event s modal.menu event time in
      let exit_mode () = next_mode s v, B.Action.NoAction in
      begin match action with
      | Menu.On(None) -> exit_mode ()
      | Menu.NoAction when Event.pressed_esc event -> exit_mode ()
      | Menu.NoAction when is_msgbox && Event.modal_dismiss event -> exit_mode ()
      | Menu.HandledEvent when is_msgbox -> exit_mode ()
      | Menu.On(Some choice) -> process_fn modal choice
      | Menu.NoAction -> v, B.Action.NoAction
      | _ -> {v with mode=build_fn {modal with menu}}, B.Action.NoAction
      end
  in
  let player_idx = C.player in
  let old_mode, old_menu = v.mode, v.menu in
  let v, backend_msg = match v.mode with
    | Normal ->
      (* Main gameplay view *)
      let menu, event = Menu.Animated.handle_event s v.menu event time in
      let v = [%up {v with menu}] in
      let v, view_action = handle_train_roster_event s v event in
      let view, view_action = match view_action with
        | `NoAction -> Mapview.handle_event s v.view event ~minimap:v.dims.minimap
        | _ -> v.view, view_action
      in
      if v.view =!= view then v.view <- view;

      let v, backend_action = match view_action with
        | `BuildTrack msg  ->
            Sound.play_sound Sound.Sound.Track_build s.sound;
            v, B.Action.BuildTrack msg
        | `RemoveTrack msg ->
            Sound.play_sound Sound.Sound.Track_remove s.sound;
            v, B.Action.RemoveTrack msg
        | `BuildFerry msg  ->
            Sound.play_sound Sound.Sound.Track_build s.sound;
            v, B.Action.BuildFerry msg
        | `BuildBridge msg ->
            let menu = build_bridge_menu s.fonts (B.get_region s.backend)
              |> Menu.MsgBox.do_open_menu s in
            let modal = make_modal menu msg in
            {v with mode=BuildBridge modal}, nobaction
        | `HighGradeTrack(msg, grade, tunnel) ->
            let menu = build_high_grade_menu ~grade ~tunnel s.fonts
              |> Menu.MsgBox.do_open_menu ~selected:(Some 0) s
            in
            let modal = make_modal menu msg in
            {v with mode=BuildHighGrade modal}, nobaction
        | `ShowTileInfo (x, y, tile) ->
            let info = Tile.Info.get (B.get_region s.backend) tile in
            let open Menu.MsgBox in
            let entries =
              let tilename = match tile with
              | City | Village ->
                  begin match B.find_close_city x y ~range:4 s.backend with
                  | Some (x,y) ->
                      let city, _ = Cities.find_exn x y s.backend.cities in
                      sp "%s (%s)" (Tile.show tile) city
                  | None -> Tile.show tile
                  end
              | _ -> Tile.show tile
              in
              let entries =
              [
                static_entry ~color:Ega.white tilename;
                static_entry ~color:Ega.white "Right-of-Way costs";
                static_entry ~color:Ega.white @@
                  sp "%s per mile" (M.print ~region:(B.get_region s.backend) info.cost);
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
                  match Goods.convert (B.get_region s.backend) good with
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
              Menu.MsgBox.make ~x:100 ~y:50 ~fonts:s.fonts entries ~font_idx:`Standard
              |> Menu.MsgBox.do_open_menu s
            in
            let modal = make_modal menu () in
            let mode = ModalMsgbox modal in
            {v with mode}, nobaction

        | `SignalMenu(x, y, dir, screen_x, screen_y) ->
            let menu =
              build_signal_menu s.fonts screen_x screen_y
              (* build_signal_menu s.fonts 10 10 (*((x + 1) * C.tile_w) (y * C.tile_h + v.dims.menu.y) *) *)
              |> Menu.MsgBox.do_open_menu s
            in
            let modal = make_modal menu (x, y, dir) in
            {v with mode=SignalMenu modal}, nobaction

        | `StationView(x, y) ->
            {v with mode=StationReport(x, y)}, nobaction

        | `DoubleTrack(double, x, y, player_idx) ->
            v, B.Action.DoubleTrack{x; y; double; player_idx}

        | `EditTrain train_idx ->
            {v with mode=TrainReport(Train_report.make s train_idx)}, nobaction

        | `HoldTrainToggle train_idx ->
            v, B.Action.TrainToggleHold {player_idx; train_idx}

        | `IncomeStatement ->
            let state = B.create_balance_sheet player_idx s.backend in
            {v with mode=Income_statement {state; start_fn=fun _ -> ()}}, nobaction

        | `TrainIncome ->
            let state = Train_income_report.create s in
            {v with mode=TrainIncome state}, nobaction

        | `BuildTrain ->
            {v with mode=BuildTrain(`ChooseEngine)}, nobaction

        | `BuildStation ->
            _build_station_mode s v, nobaction

        | `CallBroker ->
            v, B.Action.CallBroker{player_idx}

        | `NoAction -> v, nobaction

      in
      v, backend_action

    | ModalMsgbox menu ->
        handle_modal_menu_events ~is_msgbox:true menu
        (fun x -> ModalMsgbox x)
        (fun _ () -> v, nobaction)

    | Newspaper news ->
        let v = match Newspaper.handle_event s news.state event time with
          | `Exit -> next_mode s v
          | _ -> v
        in
        v, nobaction

    | BuildStation build_menu ->
        handle_modal_menu_events build_menu
        (fun x -> BuildStation x)
        (fun _ station_kind ->
            let exit_mode () = next_mode s v, nobaction in
            let x, y = Mapview.get_cursor_pos v.view in
            match Backend.check_build_station x y player_idx station_kind s.backend with
            | `Ok ->
                let backend_action = B.Action.BuildStation{x; y; kind=station_kind; player_idx} in
                next_mode s v, backend_action
                (* TODO: handle other cases *)
            | _ -> exit_mode ()
            )

    | BuildBridge build_menu ->
        handle_modal_menu_events build_menu
        (fun x -> BuildBridge x)
        (fun modal bridge_kind ->
            let msg = modal.data in
            let x, y, dir, player_idx = msg.x, msg.y, msg.dir, msg.player_idx in
            if Backend.check_build_bridge (x, y) ~dir player_idx s.backend then
                let backend_action = B.Action.BuildBridge(modal.data, bridge_kind) in
                let view = Mapview.move_const_box v.view dir 2 in
                {(next_mode s v) with view}, backend_action
            else
                next_mode s v, nobaction
            )

    | BuildHighGrade build_menu ->
        handle_modal_menu_events build_menu
        (fun x -> BuildHighGrade x)
        (fun {data={x;y;dir;player_idx} as msg;_} -> function
          | `BuildTrack ->
              let view = Mapview.move_const_box v.view dir 1 in
              {(next_mode s v) with view}, B.Action.BuildTrack msg
          | `BuildTunnel ->
              match B.check_build_tunnel (x, y) player_idx ~dir s.backend with
              | `Tunnel(length, disp_length, cost) ->
                  let menu =
                    build_tunnel_menu ~length:disp_length ~cost
                      ~region:(B.get_region s.backend) s.fonts
                    |> Menu.MsgBox.do_open_menu ~selected:(Some 0) s
                  in
                  let modal = make_modal menu (msg, length) in
                  {v with mode=BuildTunnel modal}, nobaction
              | `HitWater ->
                  make_msgbox s v "Can't tunnel under water!"
              | `HitsTrack ->
                  make_msgbox s v "Tunnel can't cross\nexisting track!"
              | `OutOfBounds ->
                  make_msgbox s v "Can't tunnel off map."
              | `TooLong ->
                  make_msgbox s v "Tunnel too long."
        )

    | BuildTunnel build_menu ->
      handle_modal_menu_events build_menu
        (fun x -> BuildTunnel x)
        (fun {data=(msg, length);_} -> function
          | true ->
              let view = Mapview.move_const_box v.view msg.dir length in
              {v with mode=Normal; view}, B.Action.BuildTunnel(msg, length)
          | _ -> {v with mode=Normal}, nobaction
        )

    | SignalMenu signal_menu ->
      handle_modal_menu_events signal_menu
        (fun x -> SignalMenu x)
        (fun {data=(x, y, dir);_} cmd ->
          {v with mode=Normal}, StationSetSignal{x; y; dir; cmd})

    | BuildTrain state ->
        let state2, status, action = Build_train.handle_event s state event time in
        let v = if state2 =!= state then {v with mode=BuildTrain(state2)} else v in
        if Utils.is_exit status then
          {v with mode=Normal}, action
        else
          v, action

    | BuildIndustry(`ChooseIndustry(industry_menu)) ->
      handle_modal_menu_events industry_menu
        (fun x -> BuildIndustry(`ChooseIndustry x))
        (fun _ wanted_tile ->
          let (x, y) = Mapview.get_cursor_pos v.view in
          let site = Tilemap.search_for_industry_site x y wanted_tile ~region:(B.get_region s.backend) s.backend.map in
          match site with
          | None -> make_msgbox ~x:144 ~y:24 s v "No suitable site found.\nTry another location."
          | Some (x, y) ->
            let view = Mapview.set_const_box_to_loc v.view ~x ~y in
            let menu = confirm_build_site_menu s.fonts |> Menu.MsgBox.do_open_menu s in
            let modal = make_modal menu (wanted_tile, x, y) in
            {v with view; mode=BuildIndustry(`ConfirmBuild modal)}, nobaction)

    | BuildIndustry(`ConfirmBuild(confirm_menu)) ->
      handle_modal_menu_events confirm_menu
        (fun x -> BuildIndustry(`ConfirmBuild x))
        (fun modal ()  ->
          let tile, x, y = modal.data in
          {v with mode=Normal}, B.Action.BuildIndustry{player_idx; x; y; tile})

    | TrainReport state ->
        let exit_state, state2, action = Train_report.handle_event s state event time in
        let v = match exit_state with
          | `Exit -> next_mode s v
          | `Stay when state =!= state2 -> {v with mode=TrainReport state2}
          | `Stay -> v
        in
        v, action

    | Stock_broker state ->
       let exit_state, state2, action = Stock_broker.handle_event s state event time in
        let v =
          if Utils.is_exit exit_state then (
            Sound.stop_music ();
            {v with mode=Normal})
          else if state =!= state2 then {v with mode=Stock_broker state2}
          else v
        in
        v, action

    | NewGoodDeliveryPickup state ->
      begin match New_delivery_pickup.handle_event state event with
      | _, `Exit -> {v with mode=Normal}, nobaction
      | state2, `Stay when state2 === state -> v, nobaction
      | state2, `Stay -> {v with mode=NewGoodDeliveryPickup(state2)}, nobaction
      end

    | Speed_record state ->
      let state2, retval, b_action = Speed_record.handle_event state event in
      let v = if state2 === state then v else {v with mode = Speed_record state2} in
      begin match retval with
      | `Exit ->
          Sound.stop_music ();
          {v with mode=Normal}, b_action
      | `Stay -> v, b_action
      end

    | Name_rr state ->
      let state2, retval, b_action = Name_rr.handle_event state event in
      let v = if state2 === state then v else {v with mode = Name_rr state2} in
      begin match retval with
      | `Exit -> {v with mode=Normal}, b_action
      | `Stay -> v, b_action
      end

    | FindCity state ->
      let state2, status = Find_city.handle_event event s.backend.cities state in
      let v = begin match status with
        | `Fail -> make_msgbox ~x:92 ~y:50 s v "No such city." |> fst
        | `Stay -> if state2 === state then v else {v with mode=FindCity state2}
        | `Return (x,y) ->
            let view = v.view
              |> Mapview.set_zoom Mapview_d.Zoom4
              |> Mapview.set_const_box_to_loc ~x ~y
            in
            {v with view; mode=Normal}
        end
      in v, nobaction

    | EngineInfo _
    | StationReport _ -> modal_screen_no_input s v event

    | StationUpgrade {transition=Some t;_} when Transition.is_finished t -> modal_screen_no_input s v event
    | StationUpgrade _ -> v, nobaction

    | Balance_sheet state ->
      (* Balance sheet at the fin period end is the last before we do housecleaning *)
      if Event.modal_dismiss event then
        let action = if state.end_of_year then B.Action.RunDelayedFn C.player else nobaction in
        let v = if state.end_of_year then v else {v with mode=Normal} in
        v, action
      else
        v, nobaction

    | TrainIncome tstate ->
        let v = begin match Train_income_report.handle_event tstate s event with
        | _, `Exit -> next_mode s v
        | tstate, `OpenTrain idx ->
            let state = Train_report.make s idx in
            {v with mode=TrainReport state; next_modes=[TrainIncome tstate]}
        | tstate2, _ when tstate2 === tstate -> v
        | tstate2, _ -> {v with mode=TrainIncome tstate2}
        end
        in v, nobaction

    | History state ->
      begin match History.handle_event state s event with
      | `Exit, _ -> next_mode s v, nobaction
      | `None, state2 when state2 === state -> v, nobaction
      | `None, state2 -> {v with mode=History state2}, nobaction
      end

    | FiredAnimation state ->
      begin match Fired_animation.handle_event event state with
      | `Exit, _ -> next_mode s v, nobaction
      | `None, state2 when state2 === state -> v, nobaction
      | `None, state2 -> {v with mode=FiredAnimation state2}, nobaction
      end

    | EndGame state ->
      begin match Endgame.handle_event s state event time with
      | `Exit, _ -> {v with mode=Normal}, nobaction
      | `QuitGame, _ -> v, B.Action.Quit_game
      | `Stay, state2 when state2 === state -> v, nobaction
      | `Stay, state2 -> {v with mode=EndGame state2}, nobaction
      end

    | GenericScreen {send_delayed_fn=true;_} ->
        if Event.modal_dismiss event then
          {v with mode=Normal}, B.Action.RunDelayedFn C.player
        else
          v, nobaction

    | SaveGame state ->
      begin match Save_game.handle_event s state event time with
      | `Stay, state2 when state2 === state -> v, nobaction
      | `Stay, state2 -> {v with mode=SaveGame state2}, nobaction
      | `Exit, _ -> {v with mode=Normal}, nobaction
      end

    | Income_statement _
    | GenericScreen {send_delayed_fn=false;_}
    | FiscalPeriodEndStocks _
    | Animation _ -> modal_screen_no_input s v event

  in
  (* See if we need to pause or unpause *)
  let pause_msgs = check_add_pause_msg old_mode old_menu v in
  v, [backend_msg] @ pause_msgs

(* Handle incoming messages from backend *)
let handle_msgs (s:State.t) v ui_msgs =
  let old_mode, old_menu = v.mode, v.menu in
  let b = s.backend in
  let main_player_idx = C.player in
  let train_arrival_msg_speed v : [`Fast | `Slow] option =
    match v.options.message_speed with
    | `Off -> None
    (* Turbo mode cancels train messges *)
    | (`Fast | `Slow) as x when not (B_options.equal_speed (B.get_speed b) `Turbo) -> Some x
    | _ -> None
  in
  let make_news ?(background=Normal) state = Newspaper {state; background} in
  let handle_msg v ui_msg =
    match v.mode with 
    | BuildTrain(`AddCars _) ->
      begin match ui_msg with
        | Ui_msg.TrainBuilt idx ->
          let state = Train_report.make s idx in
          {v with mode=TrainReport state}
        | _ -> v
      end

    | Stock_broker state ->
        let state2 = Stock_broker.handle_msg s state ui_msg in
        if state2 === state then v else {v with mode=Stock_broker state2}

    | Balance_sheet {end_of_year=true; _} ->
      begin match ui_msg with
      | FiscalPeriodEndMsgs(player_idx, msgs) ->
          if Owner.(player_idx <> main_player_idx) then v
          else
            let rate_wars, records_earnings, warnings, records, stock_msgs, job_msg, end_of_run =
              Fiscal_period_end.handle_msgs b msgs in
            let background = make_generic_screen Fiscal_period_end.render_bg in
            let modes = [] in
            let modes = List.fold_left (fun acc rate_war_msg ->
              let mode = make_generic_screen @@ Fiscal_end_rate_war.render rate_war_msg in
              let acc = mode::acc in
              match rate_war_msg.winner with
              | `Player | `Ai ->
                   let mode = make_generic_screen @@ Fiscal_end_rate_war.render_council rate_war_msg in
                   mode::acc
              | `None -> acc)
              modes rate_wars
            in
            let modes = match records_earnings with
             | Some texts ->
                 (make_news ~background @@ Newspaper.make_fancy s texts b.params)::modes
             | None -> modes
            in
            let modes = if String.length warnings > 0 then
              (make_msgbox_mode s ~x:64 ~y:40 warnings ~background)::modes else modes
            in
            let modes = if String.length records > 0 then
              (make_msgbox_mode s ~x:80 ~y:60 records ~background)::modes else modes
            in
            let stock_eval_mode, player_fired =
              let state, player_fired = Fiscal_period_end.create_stock_eval stock_msgs s in
              FiscalPeriodEndStocks state, player_fired in
            let player_fired = match player_fired with `None -> false | `PlayerFired -> true in
            let modes = stock_eval_mode::modes in
            let modes =
              if end_of_run then
                let text = sp
                  "After 100 years of faithful\n\
                  service you must retire\n\
                  from the Presidency of\n\
                  the %s."
                  (B.get_name player_idx b)
                in
                (make_msgbox_mode ~x:64 ~y:16 ~background:stock_eval_mode s text)::modes
              else modes in
            let modes = match job_msg with
              | Some _ when end_of_run -> (EndGame (Endgame.make `FinishRun s))::modes
              | Some _ when player_fired ->
                  let state = Fired_animation.make s ~fired_by:`Stockholders player_idx in
                  (EndGame (Endgame.make `Fired s))::(FiredAnimation state)::modes
              | Some job ->
                  (* New job offer *)
                  let render_fn = Job_offer.create job s |> Job_offer.render in
                  (make_generic_screen render_fn)::modes
              | None -> modes
            in
            set_modes (List.rev modes) v
      | _ -> v
      end

    | Normal ->
      let accident player_idx =
        let name = B.get_handle player_idx b in
        let text = sp "TRAIN WRECK on %s!" name in
        let num_people = (Newspaper.day_of_year b.params.time) mod 16 + 2 in
        let text2 = sp "%d persons injured." num_people in
        let text3 = "Customers Panic!" in
        make_news @@ Newspaper.make_fancy s (text, text2, text3) b.params
      in
      begin match ui_msg with 
      | DemandChanged{x; y; good; add; player_idx} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let add_remove = if add then "now\naccepts" else "no longer\naccepts" in
        let station = B.get_station (x, y) b |> Option.get_exn_or "missing station" in
        let text =
          sp "%s\n... %s %s.\n"
            (Station.get_name station)
            add_remove
            (Goods.show good)
        in
        let mode = make_news @@ Newspaper.make_simple s Newspaper.LocalNews text None in
        {v with mode}

      | (TrainArrival t) ->
          let msg_speed = train_arrival_msg_speed v in
          let v' =
            Option.map_or ~default:v
              (fun msg_speed ->
                 let time = match msg_speed with
                 | `Fast -> C.fast_message_time
                 | `Slow -> C.slow_message_time
                 in
                 Log.debug (fun f -> f "Setting train arrival message with %d time" time);
                 {v with train_arrival_msgs=v.train_arrival_msgs @ [t, ref time] }
              )
              msg_speed
          in
          if v === v' then v else v'

      | OpenStockBroker{player_idx} ->
        if Owner.(player_idx <> main_player_idx) then v else (
        Sound.play_music Sound.Music.Broker s.sound;
        let state = Stock_broker.make s in
        {v with mode=Stock_broker state})

      | StationBuilt{player_idx; loc} ->
        let x, y = loc in
        if Owner.(player_idx <> main_player_idx) then v else (
        Sound.play_sound Sound.Sound.Station_build s.sound;
        let mode = StationReport(x, y) in
        {v with mode})

      | PriorityShipmentCanceled{player_idx} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let mode = make_news @@ Newspaper.make_simple s Newspaper.LocalNews Priority_shipment.cancel_text None in
        {v with mode}

      | PriorityShipmentCreated{player_idx; shipment} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let heading, text = Priority_shipment.create_text shipment (B.get_region b) b.stations in
        let mode = make_news @@ Newspaper.make_simple s Newspaper.LocalNews ~heading text None in
        {v with mode}

      | PriorityShipmentDelivered{player_idx; shipment; bonus} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let heading, text = Priority_shipment.delivery_text shipment (B.get_region b) b.stations bonus in
        let mode = make_news @@ Newspaper.make_simple s Newspaper.LocalNews ~heading text None in
        {v with mode}

      | ImpossibleRoute a ->
          let text = sp
            "Impossible Route:\n\
             Train #%d\n\
             from %s\n\
             to %s\n\
            "
            (Train.Id.to_int a.train_idx)
            (Station_map.get_exn a.src b.stations |> Station.get_name)
            (Station_map.get_exn a.dst b.stations |> Station.get_name)
          in
          fst @@ make_msgbox ~x:100 ~y:8 s v text

      | NewCompany{opponent; city} ->
        let text = Ai.new_ai_text opponent city b.cities in
        let mode = make_news @@ Newspaper.make_simple s Newspaper.RailRoadNews text @@ Some opponent in
        {v with mode}

      | AiConnected{opponent; ai_name; src_name; tgt_name} ->
        let text = Ai.new_route_text ai_name src_name tgt_name in
        let mode = make_news @@ Newspaper.make_simple s Newspaper.RailRoadNews text @@ Some opponent in
        {v with mode}

      | AiBuildOrderFailed{player_idx; ai_name; src_name; tgt_name} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let text = Ai.build_order_fail_text ai_name src_name tgt_name in
        fst @@ make_msgbox ~x:100 ~y:8 s v text

      | IndustryBuilt{player_idx; tile} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let tile_s = Tile.show tile in
        fst @@ make_msgbox ~x:24 ~y:144 s v @@ sp "%s built." tile_s

      | AiBuysPlayerStock {opponent; takeover=true; _} ->
        let text = Ai.financial_text ~cities:b.cities ~region:b.params.region ui_msg b.ai in
        let modes = (make_news @@ Newspaper.make_simple s Newspaper.FinancialNews text @@ Some opponent)::[] in
        let state = Fired_animation.make s ~fired_by:`Management C.player in
        let modes = (FiredAnimation state)::modes in
        let modes = (EndGame (Endgame.make `Fired s))::modes in
        set_modes (List.rev modes) v

      | (AiBuySellOwnStock {opponent;_}
      | AiBuysPlayerStock {opponent;_}
      | AiSellsPlayerStock {opponent;_}) ->
        let text = Ai.financial_text ~cities:b.cities ~region:b.params.region ui_msg b.ai in
        let mode = make_news @@ Newspaper.make_simple s Newspaper.FinancialNews text @@ Some opponent in
        {v with mode}

      | AiTakesOutBond {opponent; player_idx;_} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let text = Ai.financial_text ~cities:b.cities ~region:b.params.region ui_msg b.ai in
        let mode = make_news @@ Newspaper.make_simple s Newspaper.FinancialNews text @@ Some opponent in
        {v with mode}

      | BridgeWashout {player_idx; loc; fixed} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let mode =
          if fixed then
            let text = "Bridge Repaired.\n" in
            make_news @@ Newspaper.make_simple s Newspaper.LocalNews text None
          else
            let station = Station_map.find_nearest ~player_idx ~only_proper:true loc b.stations
                |> Option.get_exn_or "no station found"
            in
            let name = Station.get_name station in
            let text = "Flood Waters Rise!", name, "Trestle Washed Away!" in
            make_news @@ Newspaper.make_fancy s text b.params in
        {v with mode}

      | NewPlayerCompany {num_shares} ->
        let mode =
          let text  = "New Railroad formed:",
                      (sp "%d,000 shares of stock" num_shares),
                      "sold to local Investors." in
          make_news @@ Newspaper.make_fancy s text b.params in
        {v with mode}

      | TrainAccident {player_idx} ->
        let state = 
          let value = if Region.is_us b.params.region then 0 else 1 in
          let input = [0, value; 1, value] in
          Pani_render.create ~input s.sound C.Pani.wreck
        in
        let next_modes = [accident player_idx] in
        {v with mode=Animation state; next_modes}

      | TrainBridgeAccident {player_idx; engine} ->
        let state = 
          let filename = if Region.is_us b.params.region then C.Pani.flood_us else C.Pani.flood_eu in
          let input = [10, 1; 0, engine.bridge_val] in
          Pani_render.create ~input s.sound filename in
        let next_modes = [accident player_idx] in
        {v with mode=Animation state; next_modes}

      | ClimateChange {climate; reason} ->
        let mode =
          let text1, text2 = Climate.text1 reason in
          let text3 = Climate.text2 climate in
          make_news @@ Newspaper.make_fancy s (text1, text2, text3) b.params in
        {v with mode}

      | FirstTrainArrives{player_idx; station} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let mode =
          let name = Station_map.get_exn station b.stations |> Station.get_name in
          let text  = "First Train Arrives",
                      (sp "in %s." name),
                      "Citizens Celebrate!" in
          make_news @@ Newspaper.make_fancy s text b.params in
        {v with mode}

      | EngineDiscovered(engine) ->
        let mode =
          let text = engine.name,
                     "Locmotive Introduced.",
                     "Bigger, Better, Faster." in
          make_news @@ Newspaper.make_fancy s text b.params in
        let next_modes=[EngineInfo(Engine_info.make engine)] in
        {v with mode; next_modes}

      | RateWarDeclared{player_idx; other_player_idx; station} ->
        let mode =
          let station_name = Station_map.get_exn station b.stations |> Station.get_name in
          let player_name = B.get_handle player_idx b in
          let other_name = B.get_handle other_player_idx b in
          let text = "Rate War Declared in",
                     (sp "%s!" station_name),
                     (sp "%s vs. %s." player_name other_name) in
          make_news @@ Newspaper.make_fancy s text b.params in
        {v with mode}

      | UnionStation{player_idx; station} ->
          if Owner.(player_idx <> main_player_idx) then v else
          let station = B.get_station station b |> Option.get_exn_or "No station found" in
          let city = Station.get_city station |> Option.get_exn_or "No city of station" in
          let city_name = Cities.name_of_loc city b.cities in
          let text = sp "Union Station in %s!\n" city_name in
          make_msgbox ~x:100 ~y:80 s v text |> fst

      | PlayerTakesControlOfOther{player_idx; other} ->
        let mode =
          let player_name = B.get_handle player_idx b in
          let other_name = B.get_handle other b in
          let text = (sp "%s take control" player_name),
                     (sp "of %s Railroad!" other_name),
                     "Wall Street amazed." in
          make_news @@ Newspaper.make_fancy s text b.params in
        {v with mode}

      | OwnerFired {player_idx; by} ->
        let mode =
          let player_name = B.get_handle player_idx b in
          let text = (sp "%s president leaves" player_name),
                     "town after meeting",
                     match by with
                     | `Stockholders -> "with stockholders."
                     | `Management -> "with new Management" in
          make_news @@ Newspaper.make_fancy s text b.params in
        {v with mode}

      | BridgeCreated {player_idx; kind} ->
        if Owner.(player_idx <> main_player_idx) then v else
        let file = match kind with
          | Wood -> Some C.Pani.wood_bridge
          | Iron -> Some C.Pani.iron_bridge
          | _ -> None in
        begin match file with
        | Some file -> {v with mode= Animation(Pani_render.create s.sound file)}
        | None -> v
        end

      | NewGoodPickedUp d ->
        if Owner.(d.player_idx <> main_player_idx) then v
        else
          let state = New_delivery_pickup.init s d.good d.station d.engine d.cars ~pickup:d.buying in
          {v with mode=NewGoodDeliveryPickup state}

      | NewGoodDelivery d ->
        if Owner.(d.player_idx <> main_player_idx) then v
        else
          let state = New_delivery_pickup.init s d.good d.dst d.engine d.cars
            ~delivery:(d.src, d.revenue, d.amount, d.speed)
          in
          {v with mode=NewGoodDeliveryPickup state}

      | SpeedRecord d ->
        if Owner.(d.player_idx <> main_player_idx) then v
        else
          let trains = B.get_trains main_player_idx b in
          let state = Speed_record.make d.speed ~src:d.src ~dst:d.dst d.train_idx trains b.stations b.cities in
          Sound.play_music End_period_song_main s.sound;
          {v with mode=Speed_record state}

      | FiscalPeriodEnd player_idx ->
          if Owner.(player_idx <> main_player_idx) then v
          else
            (* Create a chain of modes *)
            let state = B.create_balance_sheet player_idx s.backend in
            let income_stmt =
              let start_fn (s:State.t) = Sound.play_music Sound.Music.End_period_song_main s.sound in
              Income_statement {state; start_fn} in
            let bal_sheet = Balance_sheet {state; end_of_year=true} in
            let mode = make_generic_screen Fiscal_period_end.render_bg in
            Sound.play_end_year_music s.backend.params.num_fiscal_periods s.sound;
            {v with mode; next_modes=[income_stmt; bal_sheet]}

      | UpdateMap (* We don't handle it here *)
      | FiscalPeriodEndMsgs _
      | StockBroker _
      | TrainBuilt _ -> v

      end

    | _ -> v

  in
  let v = List.fold_left handle_msg v ui_msgs in
  (* Handle pausing/unpausing *)
  let pause_msg = check_add_pause_msg old_mode old_menu v in
  v, pause_msg

  (* Mostly animations. *)
let handle_tick (s:State.t) v time is_cycle =
  let nobaction = [] in
  let default = v, nobaction in
  let player_idx = C.player in
  match v.mode with
  | Normal ->
      let b = s.backend in
      let menu, menu_action = Menu.Animated.handle_tick s v.menu time in
      let v =
        let view = Mapview.handle_tick s v.view time is_cycle in
        (* decr_train_msgs *)
        let train_arrival_msgs = match v.train_arrival_msgs with
          | ((_, t)::_) as msgs when !t > 0 ->
              decr t;
              msgs
          | _::msgs -> msgs
          | [] -> []
        in
        [%up {v with view; menu; train_arrival_msgs}]
      in

      begin match menu_action with
      | On `Build_train ->
          {v with mode=BuildTrain(`ChooseEngine)}, nobaction
      | On `Build_station ->
          _build_station_mode s v, nobaction
      | On `Build_industry ->
          let menu =
            build_industry_menu s.fonts (B.get_region s.backend)
            |> Menu.MsgBox.do_open_menu s in
          let modal = make_modal menu () in
          {v with mode=BuildIndustry(`ChooseIndustry modal)}, nobaction
      | On `Survey  ->
          {v with view=Mapview.set_survey true v.view}, nobaction
      | Off `Survey ->
          {v with view=Mapview.set_survey false v.view}, nobaction
      | On `BuildTrack ->
          {v with view=Mapview.set_build_mode v.view true}, nobaction
      | On `RemoveTrack ->
          {v with view=Mapview.set_build_mode v.view false}, nobaction
      | On `ImproveStation upgrade ->
          let (x, y) as loc = Mapview.get_cursor_pos v.view in
          let station = B.get_station loc b |> Option.get_exn_or "station not found" in
          let mode = StationUpgrade{transition=None; loc; old_station=station} in
          {v with mode}, [B.Action.ImproveStation{x; y; player_idx; upgrade}]
      | On `Save_game ->
          let mode = SaveGame (Save_game.make_save s) in
          {v with mode}, nobaction
      | On `Find_city ->
          let state = Find_city.init () in
          {v with mode=FindCity state}, nobaction
      | On `Quit_game ->
          v, [B.Action.Quit_game]
      | On (`Speed speed) -> v, [B.Action.SetSpeed speed]
      | On (`Message setting) ->
          let options = {v.options with message_speed=setting} in
          {v with options}, nobaction
      | On (`News newstype) ->
          let options = v.options in
          let news = NewsTypes.add newstype options.news in
          let options = {options with news} in
          {v with options}, nobaction
      | Off (`News newstype) ->
          let options = v.options in
          let news = NewsTypes.remove newstype options.news in
          let options = {options with news} in
          {v with options}, nobaction
      | On (`Features feature) ->
          let options = v.options in
          let features = Features.add feature options.features in
          let options = {options with features} in
          {v with options}, nobaction
      | Off (`Features feature) ->
          let options = v.options in
          let features = Features.remove feature options.features in
          let options = {options with features} in
          {v with options}, nobaction
      | On (`Options option) ->
          {v with view=Mapview.update_option option true v.view }, nobaction
      | On (`Balance_sheet) ->
          let state = B.create_balance_sheet player_idx s.backend in
          {v with mode=Balance_sheet {state; end_of_year=false}}, nobaction
      | On (`Income_statement) ->
          let state = B.create_balance_sheet player_idx s.backend in
          {v with mode=Income_statement {state; start_fn=fun _ -> ()}}, nobaction
      | On (`Train_income) ->
          let state = Train_income_report.create s in
          {v with mode=TrainIncome state}, nobaction
      | On (`Stocks) ->
          {v with mode=make_generic_screen Stock_graph.render}, nobaction
      | On (`Accomplishments) ->
          {v with mode=make_generic_screen Accomplishments.render}, nobaction
      | On (`History) ->
          {v with mode=History (History.create s)}, nobaction
      | On (`Efficiency_report) ->
          {v with mode=make_generic_screen Efficiency_report.render}, nobaction
      | On (`Call_broker) ->
          v, [B.Action.CallBroker{player_idx}]
      | On (`Name_rr) ->
          let state = Name_rr.init b.stations b.cities player_idx b in
          {v with mode=Name_rr state}, nobaction
      | On (`Retire) ->
          let mode = EndGame(Endgame.make `RetireEarly s) in
          {v with mode}, nobaction
      | On (`Cheat x) ->
          v, [B.Action.Cheat(C.player, x)]
      | Off (`Options option) ->
          {v with view=Mapview.update_option option false v.view}, nobaction
      | On ((`Repeat_message | `Upgrade_bridge | `Reality_level _| `Display _ ) as ma) ->
          Printf.printf "Unhandled message %s\n" (show_menu_action ma);
          v, nobaction
      | _ -> v, nobaction
      end

  | BuildTrain(`AddCars state) ->
      let state2 = Build_train.AddCars.handle_tick s state time in
      if state === state2 then default
      else {v with mode=BuildTrain(`AddCars state2)}, nobaction
  | TrainReport state ->
      let status, state2, baction = Train_report.handle_tick s state time in
      let v = match status with
        | `Exit -> next_mode s v
        | `Stay when state2 =!= state -> {v with mode=TrainReport state2}
        | `Stay -> v
      in
      v, baction
  | Animation state ->
      let state2, _ = Pani_render.handle_tick time state in
      if state2 === state then default
      else {v with mode=Animation state2}, nobaction

  | NewGoodDeliveryPickup d ->
      let d2 = New_delivery_pickup.handle_tick s time d in
      if d2 === d then default
      else {v with mode=NewGoodDeliveryPickup d}, nobaction

  | History state ->
     let state2 = History.handle_tick s time state in
     if state2 === state then default
     else {v with mode=History state2}, nobaction

  | StationUpgrade ({transition=None; old_station; loc} as state) ->
    (* Iniitialize transition *)
    let old_render_fn win = Station_report.render win s ~station:old_station loc ~show_demand:false in
    (* Note: this must happen after backend has updated. Might need a message here *)
    let render_fn win = Station_report.render win s loc ~show_demand:false in
    let transition = Transition.make s.win s.random ~wait_time:0 ~old_render_fn ~render_fn |> Option.some in
    let mode = StationUpgrade {state with transition} in
    {v with mode}, nobaction

  | StationUpgrade ({transition=Some t; _} as state) ->
    let status, t2 = Transition.handle_tick time t in
    begin match status with
    | `Stay when t2 =!= t -> {v with mode=StationUpgrade {state with transition = Some t2}}, nobaction
    | `Stay | `Exit -> default (* we don't allow exit by ticks here *)
    end

  | Stock_broker state ->
    let status, state2, actions = Stock_broker.handle_tick s state time in
    let v =
      if Utils.is_exit status then (
        Sound.stop_music ();
        next_mode s v
      ) else if state2 =!= state then {v with mode=Stock_broker state2}
      else v
    in
    v, actions

  | FiredAnimation state ->
    let state2 = Fired_animation.handle_tick time state in
    if state2 === state then default
    else {v with mode=FiredAnimation state2}, nobaction

  | _ -> default

let draw_train_roster win (s:State.t) v =
  train_roster_iter s v
  (fun y_bot idx ->
    let train = B.get_train (Trainmap.Id.of_int idx) C.player s.backend in
    let x = v.dims.train_ui.x + 1 in
    let y = y_bot in
    let x2 = v.dims.train_ui.x + v.dims.train_ui.w - 1 in
    let _draw_speed_line =
      let color = if Train.get_train_hold train then Ega.bred else Ega.dgray in
      R.draw_line win ~x1:x ~y1:y ~x2 ~y2:y ~color;
      let x1 = x2 - (Train.get_speed train) * 2 in
      R.draw_line win ~x1 ~y1:y ~x2 ~y2:y ~color:Ega.bgreen;
    in
    let _draw_engine =
      let color = if Train.holds_priority_shipment train then Ega.bgreen else Ega.black in
      R.draw_line win ~x1:(x+3) ~y1:(y-3) ~x2:(x+3) ~y2:y ~color;
      R.draw_line win ~x1:(x+4) ~y1:(y-2) ~x2:(x+4) ~y2:(y-1) ~color;
      R.draw_rect win ~x:(x+5) ~y:(y-3) ~w:2 ~h:4 ~color ~fill:true;
    in
    let _draw_cars =
      List.fold_left (fun x car ->
        let full = Train.Car.get_amount car > C.car_amount / 2 in
        let freight = Train.Car.get_good car in
        Ui_common.draw_ui_car win ~x ~y:(y-2) ~full freight;
        x + 5)
      (x + 8)
      train.cars
    in
    let _write_destination =
      let loc = Train.get_dest train in
      let station = Station_map.get_exn loc s.backend.stations in
      let short_name = Station.get_short_name station in
      Fonts.Render.write win s.fonts ~color:Ega.white ~idx:`Tiny short_name ~x:(x2-11) ~y:(y-4)
    in
    ()
  )

let render_main win (s:State.t) v =
  let dims = v.dims in
  let player_idx = C.player in
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

  let draw_train_arrival_msg (msg:Ui_msg.train_arrival_msg) =
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
      buf_add @@ string_of_int (Train.Id.to_int msg.train_num + 1);
      buf_add ")\n";
      List.iter (fun (good, amount) ->
         buf_add @@ Goods.short_descr_of good amount)
        msg.goods_amount;
      buf_add "\nRev: ";
      buf_add @@ M.print msg.revenue;
      Buffer.contents b
    in
    let x, y = (dims.minimap.x+1), (dims.minimap.y+1) in
    let h, w = dims.minimap.h - 1, dims.minimap.w - 1 in
    R.draw_rect win ~x ~y ~h ~w ~color:Ega.bblue ~fill:true;
    Fonts.Render.write win s.fonts ~color:Ega.white ~idx:`Tiny ~x:258 ~y:12 msg_s
  in
  begin match v.train_arrival_msgs with
  | (msg, _)::_ -> draw_train_arrival_msg msg
  | _ -> ()
  end;

  (* Info bar *)
  let y = y + dims.minimap.h in
  R.draw_rect win ~x ~y ~h:dims.infobar.h ~w:dims.ui.w ~color:Ega.white ~fill:true;

  if Backend.broker_timer_active player_idx s.backend then
    Fonts.Render.write win s.fonts ~color:Ega.bgreen ~idx:`Standard ~x:256 ~y:66 "B";

  let region = B.get_region s.backend in
  let cash = B.get_cash player_idx s.backend in
  let cash_s = M.print ~show_neg:false ~spaces:6 ~region cash in
  let color = if M.(cash < zero) then Ega.bred else Ega.black in
  Fonts.Render.write win s.fonts ~color ~idx:`Standard ~x:264 ~y:66 cash_s;

  let month, year = B.get_date s.backend in
  let date_s = sp "%s %d" (Utils.str_of_month month) year in
  Fonts.Render.write win s.fonts ~color:Ega.black ~idx:`Standard ~x:264 ~y:74 date_s;

  (* Train area *)
  let y = y + dims.infobar.h in
  R.draw_rect win ~x:(x+1) ~y:y ~h:dims.train_ui.h ~w:(dims.ui.w-1) ~color:Ega.bblue ~fill:true;
  draw_train_roster win s v;

  (* Priority time remaining *)
  let draw_priority () =
    match Backend.get_priority_shipment player_idx s.backend with
    | Some priority ->
      let bonus = Priority_shipment.compute_bonus priority @@ B.get_params s.backend in
      let bonus_s = sp "bonus: %d,000" @@ M.to_int bonus in
      Fonts.Render.write win s.fonts ~color:Ega.white ~idx:`Tiny ~x:258 ~y:194 bonus_s;
    | _ -> ()
  in
  draw_priority ();

  (* Menu bar *)
  Menu.Animated.render win s v.menu;
  ()

let should_render_mouse = function
  | _ -> true

let render_mouse win textures =
  let cursor_tex = Hashtbl.find textures.Textures.misc `Cursor in
  R.draw_cursor win cursor_tex

let render (win:R.window) (s:State.t) v =
  (* Msgboxes *)
  let rec render_mode = function
    | Normal ->
        render_main win s v
    | ModalMsgbox modal ->
        render_mode modal.background;
        Menu.MsgBox.render win s modal.menu
    | Newspaper news ->
        render_mode news.background;
        Newspaper.render win s news.state
    | BuildStation modal ->
        render_main win s v;
        Menu.MsgBox.render win s modal.menu
    | BuildIndustry(`ChooseIndustry modal) ->
        render_main win s v;
        Menu.MsgBox.render win s modal.menu
    | BuildIndustry(`ConfirmBuild modal) ->
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
    | StationUpgrade{transition=Some t; _} ->
        Transition.render win t
    | StationUpgrade{transition=None; old_station; loc} ->
        Station_report.render win s ~station:old_station loc ~show_demand:false
    | EngineInfo state ->
        Engine_info.render win state ~fonts:s.fonts ~textures:s.textures ~region:(B.get_region s.backend)
    | BuildTrain(state) ->
        Build_train.render win s state
    | TrainReport state ->
        Train_report.render win s state
    | Stock_broker state ->
        Stock_broker.render win s state
    | Balance_sheet {state;_} ->
        Balance_sheet_view.render win s state
    | Income_statement {state;_} ->
        Income_statement_view.render win s state
    | Animation state ->
        Pani_render.render win state
    | NewGoodDeliveryPickup d ->
        New_delivery_pickup.render win s d
    | Speed_record d ->
        Speed_record.render win s d
    | Name_rr state ->
        Name_rr.render win s state
    | FindCity state ->
       render_main win s v;
       Find_city.render win s.fonts state
    | FiscalPeriodEndStocks state ->
       Fiscal_period_end.render_stock_eval win state  s
    | TrainIncome state ->
      Train_income_report.render win state s
    | History state ->
       History.render win state s
    | EndGame state ->
       Endgame.render win state s
    | FiredAnimation state ->
       Fired_animation.render win s state
    | SaveGame state ->
        Save_game.render win s state
    | GenericScreen {render_fn; _} ->
       render_fn win s
  in
  render_mode v.mode;
  if should_render_mouse v.mode then
    render_mouse win s.textures

