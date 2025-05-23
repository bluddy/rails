open Containers
module R = Renderer
module B = Backend
open Train_report_d
open Utils.Infix
module Vector = Utils.Vector
module C = Constants
module T = Train

open Train_route_orders

(* The edit train screen *)

let nobaction = B.Action.NoAction

let menu_h = 8

let make_menu fonts train_idx ~engine_make ~engines ~year =
  let open Menu in
  let engine_menu =
    let open MsgBox in
    let engine_info_menu =
      let engine_options =
        Engine.available_at_year engines ~year
        |> List.map (fun (engine:Engine.t) -> make_entry engine.name @@ `Action (`EngineInfo engine.make))
      in
      make ~fonts ~x:25 ~y:12 @@
      (make_entry "This Engine" @@ `Action(`EngineInfo engine_make))
      ::engine_options
    in
    make ~fonts ~x:20 ~y:8 [
      make_entry "&Engine Info" @@ `MsgBox engine_info_menu; 
      make_entry "&Replace Engine" @@ `Action `ReplaceEngine;
      make_entry "&Retire Train" @@ `Action `RetireTrain;
    ]
    in
  let train_type_menu =
    let open MsgBox in
    let check_type typ (s:State.t) =
      let train =
        let trains = Backend.get_player C.player s.backend |> Player.get_trains in
        Trainmap.get trains train_idx in
      Train.equal_train_type typ train.typ
    in
    make ~fonts ~x:100 ~y:8 [
      make_entry "&Local" @@ `Checkbox(`Type T.Local, check_type T.Local);
      make_entry "&Through" @@ `Checkbox(`Type T.Through, check_type T.Through);
      make_entry "&Express" @@ `Checkbox(`Type T.Express, check_type T.Express);
      make_entry "&Limited" @@ `Checkbox(`Type T.Limited, check_type T.Limited);
    ]
  in
  let route_map_menu =
    let open MsgBox in
    make ~fonts ~x:160 ~y:8 [
      make_entry "Show map" @@ `Action `ShowMap;
    ]
  in
  let titles =
    let open Menu.Title in
    [
      make ~fonts ~x:8 ~y:1 "&Engine" engine_menu;
      make ~fonts ~x:72 ~y:1 "&Train type" train_type_menu;
      make ~fonts ~x:160 ~y:1 "&Route map" route_map_menu;
    ]
  in
  Menu.Global.make ~menu_h titles

let open_car_menu (s:State.t) stop =
  let open Menu.MsgBox in
  let menu =
    (Build_train.AddCars.create_menu ~fonts:s.fonts (B.get_region s.backend)
      ~suffix:[make_entry "Caboose only" (`Action `Caboose)])
      |> do_open_menu s
  in
  Some(menu, stop)

let make (s:State.t) train_idx =
  let train =
    let trains = Backend.get_player C.player s.backend |> Player.get_trains in
    Trainmap.get trains train_idx in
  let menu = make_menu s.fonts train_idx ~engines:s.backend.engines ~year:(B.get_year s.backend) ~engine_make:train.engine.make in
  {
    train=train_idx;
    menu;
    car_menu=None;
    screen=Normal;
  }

let render win (s:State.t) (v:State.t t) : unit =
  match v.screen with
  | TrainRouteOrders station_map ->
      Train_route_orders.render win s station_map

  | EngineInfo state ->
      Engine_info.render win state ~fonts:s.fonts ~textures:s.textures
        ~region:(B.get_region s.backend)

  | ChooseEngine ->
      Choose_engine.render win s ~engines:s.backend.engines
        ~year:(B.get_year s.backend)

  | Normal ->
    let train = Backend.get_train s.backend v.train ~player:0 in
    let write color = Fonts.Render.write win s.fonts ~color ~idx:4 in

    (* Draw screen background *)
    R.paint_screen win ~color:Ega.white;
    R.draw_rect win ~x:2 ~y:9 ~color:Ega.black ~w:316 ~h:107 ~fill:false;
    R.draw_line win ~color:Ega.black ~x1:2 ~y1:49 ~x2:316 ~y2:49;
    R.draw_line win ~color:Ega.black ~x1:0 ~y1:115 ~x2:319 ~y2:115;
    R.draw_line win ~color:Ega.black ~x1:0 ~y1:116 ~x2:319 ~y2:116;

    let open Printf in
    let train_info_s = match train.name with
      | Some name -> sprintf "Train #%d: %s" (Trainmap.Id.to_int v.train + 1) name
      | _ -> sprintf "Train #%d: %s %s"
        (Trainmap.Id.to_int v.train + 1)
        (Freight.show train.freight) 
        (Train.show_train_type train.typ)
    in
    let train_loc = (train.x, train.y) in
    let train_loc_s = match train.state with
      | Traveling _ ->
          let station = Station_map.find_nearest s.backend.stations train_loc
            |> Option.get_exn_or "must have station"
          in
          "near "^Station.get_name station
      | _ ->
          let station = Station_map.get_exn (train.x / C.tile_w, train.y / C.tile_h) s.backend.stations in
          sprintf ("at %s") (Station.get_name station)
    in
    let maintenance = Train.display_maintenance train
      |> Utils.show_cash ~region:(B.get_region s.backend) in
    let engine_data_s = sprintf "(%s/%s)" train.engine.name maintenance in
    let status_s = match train.state with
      | LoadingAtStation _ -> "Speed: unloading/loading"
      | HoldingAtStation -> "Speed: holding"
      | WaitingForFullLoad -> "Speed: waiting for full load"
      | StoppedAtSignal _ -> "Speed: stopped at signal"
      | Traveling _ ->
          sprintf "Speed: %d mph, bound for %s"
          (Train.display_speed train)
          (let loc = Train.get_dest train in
           Station_map.get_exn loc s.backend.stations 
           |> Station.get_name)
    in
    let str = sprintf "%s\n%s  %s\n%s"
      train_info_s train_loc_s engine_data_s status_s
    in
    write Ega.black ~x:8 ~y:12 str;

    (* Draw current train engine *)
    let engine_tex = Hashtbl.find s.textures.route_engine @@ train.engine.make in
    R.Texture.render ~x:3 ~y:40 win engine_tex;

    (* Draw current cars *)
    let draw_cars cars ~x ~y extract_fn =
      match cars with
      | [] ->
          let caboose = Hashtbl.find s.textures.route_cars `Caboose in
          R.Texture.render ~x ~y win caboose

      | cars ->
        ignore @@
          List.fold_left (fun x car_data ->
            let car = extract_fn car_data in
            let car_tex = Hashtbl.find s.textures.route_cars (`CarOld car) in
            R.Texture.render ~x ~y win car_tex;
            x + car_tex.w
          ) x cars
    in
    draw_cars train.cars ~x:66 ~y:41 Train.Car.get_good;
    
    write Ega.black ~x:292 ~y:40 "Exit";

    (* Write contents of cars *)
    let car_desc_s =
      List.fold_left (fun acc_s car ->
        match Train.Car.get_load car with
        | Some (good, amount, loc) when amount >= 4 ->
            let s1 = Goods.descr_of good amount in
            let s2 = Station_map.get_exn loc s.backend.stations 
              |> Station.get_name 
            in
            Printf.sprintf "%s%s from %s\n" acc_s s1 s2
        | _ -> acc_s)
        ""
        train.cars
    in
    write Ega.black ~x:7 ~y:51 car_desc_s;

    let rev period =
      Train.get_revenue train period
      |> Utils.show_cash ~spaces:6 ~region:(B.get_region s.backend)
    in
    let period = B.get_period s.backend in
    let rev_l = rev period in
    let rev_r = rev @@ Utils.other_period period in
    write Ega.green ~x:7 ~y:118 rev_l;
    write Ega.green ~x:256 ~y:118 rev_r;

    write Ega.black ~x:105 ~y:118 "TRAIN ORDERS";

    let write_station ?(wait=`NoWait) (stop:Train.stop) ~i ~y =
      let station =
        Station_map.get_exn (stop.x, stop.y) s.backend.stations
      in
      let color = match i with
        | Some i ->
            write Ega.gray ~x:3 ~y @@ Printf.sprintf "%d." (i+1);
            if i = train.stop then Ega.black else Ega.gray
        | None ->
            Ega.black
      in
      begin match wait with
      | `Wait -> write color ~x:12 ~y "W"
      | _ -> ()
      end;
      write color ~x:24 ~y (Station.get_name station)
    in

    let draw_cars_option (stop:Train.stop) ~y =
      match stop.consist_change with
      | None -> write Ega.gray ~x:168 ~y "no changes"
      | Some cars -> draw_cars cars ~x:160 ~y Utils.id
    in

    (* Priority *)
    Renderer.draw_rect win ~x:2 ~y:127 ~w:315 ~h:10 ~color:Ega.yellow ~fill:true;
    write Ega.black ~x:8 ~y:128 "Priority Orders:";
    write Ega.black ~x:160 ~y:128 "Priority Consist:";
    write Ega.gray ~x:3 ~y:138 "P";
    R.draw_line win ~color:Ega.black ~x1:160 ~y1:147 ~x2:312 ~y2:147;

    begin match train.priority_stop with
    | None ->
        write Ega.black ~x:29 ~y:138 "---";
        write Ega.gray ~x:168 ~y:138 "no changes"
    | Some stop ->
        write_station stop ~i:None ~y:138;
        draw_cars_option stop ~y:138
    end;

    (* Stops *)
    R.draw_rect win ~x:2 ~y:148 ~w:315 ~h:10 ~color:Ega.bgreen ~fill:true;
    write Ega.black ~x:8 ~y:149 "Scheduled Stops:";
    write Ega.black ~x:160 ~y:149 "New Consist:";

    (* Write stop names *)
    let last_full_stop, y =
      Vector.fold (fun (i, y) (wait, (stop:Train.stop)) ->
        write_station stop ~wait ~i:(Some i) ~y;
        draw_cars_option stop ~y;
        R.draw_line win ~color:Ega.black ~x1:160 ~y1:(y+9) ~x2:312 ~y2:(y+9);
        (i+1, y+10)
      )
      (0, 159)
      train.route
    in
    (* Fill in blank stops *)
    ignore @@
    Iter.fold (fun y _ ->
      write Ega.gray ~x:29 ~y "---";
      y + 10)
      y
      Iter.(last_full_stop -- Train.max_stops);

    (* Car menu *)
    begin match v.car_menu with
    | None -> ()
    | Some (car_menu, _) -> Menu.MsgBox.render win s car_menu
    end;

    (* Menu bar - last so we draw over all else *)
    Menu.Global.render win s s.fonts v.menu ~w:s.ui.dims.screen.w ~h:8;
    ()

let _find_clicked_stop (train:ro Train.t) click_y =
    let ystart = 167 in
    let line_h = 10 in
    Vector.foldi (fun i acc _ ->
      match acc with
      | None when click_y < ystart + i * line_h -> Some i
      | x -> x)
    None
    train.route

let handle_event (s:State.t) v (event:Event.t) =
  match v.screen, v.car_menu with
  | TrainRouteOrders state, _ ->
      let exit, state2, b_action = Train_route_orders.handle_event s state event in
      let v = if state =!= state2 then {v with screen=TrainRouteOrders state2} else v in
      let v = if exit then {v with screen=Normal} else v in
      false, v, b_action

  | EngineInfo _, _ ->
    let v = match Engine_info.handle_event event with
      | `Exit -> {v with screen=Normal}
      | _ -> v
    in
    false, v, nobaction

  | ChooseEngine, _ ->
    begin match Choose_engine.handle_event event s.backend.engines ~year:(B.get_year s.backend) with
    | Some engine ->
      let menu =
        make_menu s.fonts v.train ~engines:s.backend.engines ~year:(B.get_year s.backend) ~engine_make:engine.make
      in
      let baction =
        B.Action.TrainReplaceEngine {train=v.train; engine=engine.make; player=0}
      in
      false, {v with screen=Normal; menu}, baction
    | _ -> 
      false, v, nobaction
    end

  | Normal, (Some(car_menu, stop) as current) ->
      (* Car menu selection open *)
      let car_menu2, action = Menu.MsgBox.update s car_menu event in
      let car_menu, b_action = match action with
        | Menu.On(`Caboose) ->
            None, Backend.Action.RemoveAllStopCars({train=v.train; stop; player=0})
        | Menu.On(`AddCar car) ->
            None, Backend.Action.AddStopCar({train=v.train; stop; car; player=0})
        | Menu.NoAction when Event.pressed_esc event ->
            None, nobaction
        | Menu.On(`Done) ->
            None, nobaction
        | _ ->
            let car_menu = if car_menu2 =!= car_menu then Some(car_menu2, stop) else current in
            car_menu, nobaction
      in
      let v = [%up {v with car_menu}] in
      false, v, b_action

  | Normal, _ ->
      let menu, action, event = Menu.Global.update s v.menu event in
      let train = Backend.get_train s.backend v.train ~player:0 in
      let line_h = 10 in
      let xstart = 160 in
      let car_w = 20 in
      let make_car_msg inner_msg cars x =
          List.foldi (fun acc j _ -> match acc with
            | `AddCarMenu _ when x < xstart + (j + 1) * car_w ->
                `DeleteCar (inner_msg, j)
            | _ -> acc)
          (`AddCarMenu inner_msg)
          cars
      in
      let handle_car_msg = function
        | `AddCarMenu stop ->
            false, v.screen, open_car_menu s stop, nobaction
        | `DeleteCar (stop, car) ->
            let b_action = Backend.Action.RemoveStopCar{train=v.train; stop; car; player=0} in
            false, v.screen, None, b_action
        | `None ->
            false, v.screen, None, nobaction
      in

      let exit, screen, car_menu, bk_action =
        match action, event with
          (* Global menu choice: route map view *)
        | Menu.On(`ShowMap), _ ->
            let screen = TrainRouteOrders (Train_route_orders.make s.backend.graph v.train `ShowRoute) in
            false, screen, None, nobaction

        | Menu.On(`Type typ), _ ->
            false, v.screen, None, B.Action.TrainSetType{train=v.train; typ; player=0}

        | Menu.On(`EngineInfo engine_make), _ ->
            let engine = Engine.t_of_make s.backend.engines engine_make in
            let screen = EngineInfo (Engine_info.make engine) in
            false, screen, None, nobaction

        | Menu.On(`RetireTrain), _ ->
            true, v.screen, None, B.Action.RemoveTrain {idx=v.train; player=0}

        | Menu.On(`ReplaceEngine), _ ->
            false, ChooseEngine, None, nobaction

          (* Click on priority stop -> open route map *)
        | _, MouseButton {x; y; button=`Left; down=true; _} when x <= 120 && y >= 137 && y <= 147 ->
            let screen = 
              TrainRouteOrders (Train_route_orders.make s.backend.graph v.train `EditPriority)
            in
            false, screen, None, nobaction

          (* Click on a stop -> open route map *)
        | _, MouseButton {x; y; button=`Left; down=true; _} when x >= 24 && x <= 120 && y >= 159 ->
            let ystart = 167 in
            let res =
              Iter.foldi (fun acc i _ -> match acc with
                | None when y <= ystart + i * line_h -> Some i
                | x -> x)
              None
              Iter.(0 -- Vector.length (train.route))
            in
            begin match res with
            | Some i ->
                let screen =
                  TrainRouteOrders (Train_route_orders.make s.backend.graph v.train @@ `EditStop i)
                in
                false, screen, None, nobaction
            | _ -> false, v.screen, None, nobaction
            end

          (* Click next to station to wait or unwait *)
        | _, MouseButton {x; y; button=`Left; down=true; _} when x >= 9 && x <= 23 && y >= 159 ->
          let stop = _find_clicked_stop train y in
          let b_action = match stop with
            | Some stop ->
                Backend.Action.TrainToggleStopWait{train=v.train; stop; player=0}
            | None -> nobaction
          in
          false, v.screen, None, b_action

          (* Click on car to delete, or space in priority stop to open the menu *)
        | _, MouseButton {x; y; button=`Left; down=true; _} when x >= 160 && y >= 137 && y <= 147 ->
            let msg = match train.priority_stop with
              | Some {consist_change=Some cars;_} -> make_car_msg `Priority cars x
              | Some {consist_change=None;_} -> `AddCarMenu `Priority
              | _ -> `None
            in
            handle_car_msg msg

        | _, MouseButton {x; y; button=`Left; down=true; _} when x >= 160 && y >= 159 ->
            let stop = _find_clicked_stop train y in
            let msg = Option.map_or ~default:`None
              (fun i ->
                 let _, stop = Train.get_stop train i in
                 match stop.consist_change with
                 | None -> `AddCarMenu (`Stop i)  (* currently "No Change" *)
                 | Some cars -> make_car_msg (`Stop i) cars x)
              stop
            in
            handle_car_msg msg

        | _ ->
          false, v.screen, None, nobaction
      in
      let v = [%up {v with menu; screen; car_menu}] in
      let exit = Event.pressed_esc event || exit in
      exit, v, bk_action

let handle_tick v time =
  begin match v.screen with
  | TrainRouteOrders state -> Train_route_orders.handle_tick state time
  | _ -> ()
  end;
  v

