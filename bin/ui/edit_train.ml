open Containers
module R = Renderer
module B = Backend
open Edit_train_d
open Utils.Infix

open Station_map_ui

(* The edit train screen *)

let nobaction = B.Action.NoAction

let menu_h = 8

let make_menu fonts =
  let open Menu in
  let engine_menu =
    let open MsgBox in
    make ~fonts [
      make_entry "Dummy" @@ `Action `ShowMap;
      make_entry "Dummy" @@ `Action `ShowMap;
    ]
    in
  let train_type_menu = engine_menu in
  let route_map_menu =
    let open MsgBox in
    make ~fonts ~x:160 ~y:8 [
      make_entry "Dummy" @@ `Action `ShowMap;
      make_entry "Dummy" @@ `Action `ShowMap;
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

let open_car_menu (s:State.t) i =
  let open Menu.MsgBox in
  let menu =
    (Build_train.AddCars.create_menu ~fonts:s.fonts s.backend.region
      ~suffix:[make_entry "Caboose only" (`Action `Caboose)])
      |> do_open_menu s
  in
  Some(menu, i)

let make (s:State.t) train =
  let menu = make_menu s.fonts in
  {
    train;
    menu;
    car_menu=None;
    screen=Normal;
  }

let render win (s:State.t) v : unit =
  match v.screen with
  | StationMap station_map ->
      Station_map_ui.render win s station_map

  | Normal ->
    let train = Backend.get_train s.backend v.train in
    let write color = Fonts.Render.write win s.fonts ~color ~idx:4 in

    (* Draw screen background *)
    R.paint_screen win ~color:Ega.white;
    R.draw_rect win ~x:2 ~y:9 ~color:Ega.black ~w:316 ~h:107 ~fill:false;
    R.draw_line win ~color:Ega.black ~x1:2 ~y1:49 ~x2:316 ~y2:49;
    R.draw_line win ~color:Ega.black ~x1:0 ~y1:115 ~x2:319 ~y2:115;
    R.draw_line win ~color:Ega.black ~x1:0 ~y1:116 ~x2:319 ~y2:116;

    (* TODO: make these things dynamic *)
    let open Printf in
    let line1 = sprintf "Train #%d: %s %s\n" v.train (Goods.show_freight train.freight) "Limited" in
    let line2 = sprintf "near %s (%s/%s)\n" "Wausau" train.engine.name "$4,000" in
    let line3 = sprintf "Speed: %d mph, bound for %s" 25 "Wausau" in
    write Ega.black ~x:8 ~y:12 (line1^line2^line3);

    (* Draw current train engine *)
    let engine_tex = Hashtbl.find s.textures.route_engine @@ train.engine.make in
    R.Texture.render ~x:3 ~y:40 win engine_tex;

    (* Draw current cars *)
    let draw_cars cars ~x ~y extract_fn =
      ignore @@
        List.fold_left (fun x car_data ->
          let car = extract_fn car_data in
          let car_tex = Hashtbl.find s.textures.route_cars (`CarOld car) in
          R.Texture.render ~x ~y win car_tex;
          x + car_tex.w
        ) x cars
    in
    draw_cars train.cars ~x:66 ~y:41 fst;
    
    write Ega.black ~x:292 ~y:40 "Exit";

    write Ega.black ~x:105 ~y:118 "TRAIN ORDERS";

    let draw_cars_option (stop:Train.stop) ~y =
      match stop.cars with
      | None -> write Ega.gray ~x:168 ~y "no changes"
      | Some cars -> draw_cars cars ~x:160 ~y Utils.id
    in

    (* Priority *)
    Renderer.draw_rect win ~x:2 ~y:127 ~w:315 ~h:10 ~color:Ega.yellow ~fill:true;
    write Ega.black ~x:8 ~y:128 "Priority Orders:";
    write Ega.black ~x:160 ~y:128 "Priority Consist:";
    write Ega.gray ~x:8 ~y:138 "P";
    R.draw_line win ~color:Ega.black ~x1:160 ~y1:147 ~x2:312 ~y2:147;

    begin match train.priority with
    | None ->
        write Ega.black ~x:29 ~y:138 "---";
        write Ega.gray ~x:168 ~y:138 "no changes"
    | Some stop ->
        draw_cars_option stop ~y:138
    end;

    (* Stops *)
    R.draw_rect win ~x:2 ~y:148 ~w:315 ~h:10 ~color:Ega.bgreen ~fill:true;
    write Ega.black ~x:8 ~y:149 "Scheduled Stops:";
    write Ega.black ~x:160 ~y:149 "New Consist:";

    (* Write stop names *)
    let n, y =
      List.fold_left (fun (i, y) (stop:Train.stop) ->
        let station = Loc_map.get_exn s.backend.stations stop.x stop.y in
        write Ega.gray ~x:8 ~y @@ Printf.sprintf "%d." (i+1);
        let color = if i = train.target_stop then Ega.black else Ega.gray in
        write color ~x:24 ~y station.name;
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
      Iter.(n -- Train.max_stops);

    (* Car menu *)
    begin match v.car_menu with
    | None -> ()
    | Some (car_menu, _) -> Menu.MsgBox.render win s car_menu
    end;

    (* Menu bar - last so we draw over all else *)
    Menu.Global.render win s s.fonts v.menu ~w:s.ui.dims.screen.w ~h:8;

    ()

let handle_event (s:State.t) v (event:Event.t) =
  match v.screen, v.car_menu with
  | StationMap state, _ ->
      let exit, state2, b_action = Station_map_ui.handle_event s state event in
      let v = if state =!= state2 then {v with screen=StationMap state2} else v in
      let v = if exit then {v with screen=Normal} else v in
      false, v, b_action

  | Normal, (Some(car_menu, stop) as current) ->
      (* Car menu selection open *)
      let car_menu2, action = Menu.MsgBox.update s car_menu event in
      let car_menu, b_action = match action with
        | Menu.On(`Caboose) ->
            None, Backend.Action.RemoveAllStopCars({train=v.train; stop})
        | Menu.On(`AddCar car) ->
            None, Backend.Action.AddStopCar({train=v.train; stop; car})
        | Menu.NoAction when Event.pressed_esc event ->
            None, nobaction
        | Menu.On(`Done) ->
            None, nobaction
        | _ ->
            let car_menu = if car_menu2 =!= car_menu then Some(car_menu2, stop) else current in
            car_menu, nobaction
      in
      let v = if car_menu =!= v.car_menu then {v with car_menu} else v in
      false, v, b_action

  | Normal, _ ->
      let menu, action, event = Menu.Global.update s v.menu event in
      let train = Backend.get_train s.backend v.train in
      let line_h = 10 in
      let screen, car_menu, bk_action =
        match action, event with
          (* Global menu choice: route map view *)
        | Menu.On(`ShowMap), _ ->
            let screen = StationMap (Station_map_ui.make s.backend.graph v.train `ShowRoute) in
            screen, None, nobaction

          (* Click on a stop -> open route map *)
        | _, MouseButton {x; y; button=`Left; down=true; _} when x <= 120 && y >= 158 ->
            let ystart = 167 in
            let res =
              Iter.foldi (fun acc i _ -> match acc with
                | None when y <= ystart + i * line_h ->
                    Some i
                | x -> x)
              None
              Iter.(0 -- List.length(train.route))
            in
            begin match res with
            | Some i ->
                let screen =
                  StationMap (Station_map_ui.make s.backend.graph v.train @@ `EditStop i)
                in
                screen, None, nobaction
            | _ -> v.screen, None, nobaction
            end

          (* Click on car to delete, or space in stop to open the menu *)
        | _, MouseButton {x; y; button=`Left; down=true; _} when x >= 160 && y >= 159 ->
            let ystart, xstart, car_w = 167, 160, 20 in
            let res =
              List.foldi (fun acc i (stop:Train.stop) -> match acc with
                | `None when y < ystart + i * line_h ->
                    begin match stop.cars with
                    | None -> `AddCarMenu i  (* currently "No Change" *)
                    | Some cars ->
                        List.foldi (fun acc j _ -> match acc with
                          | `AddCarMenu _ when x < xstart + (j + 1) * car_w -> `DeleteCar (i, j)
                          | _ -> acc)
                        (`AddCarMenu i)
                        cars
                    end
                | x -> x)
              `None
              train.route
            in
            begin match res with
            | `AddCarMenu i ->
                v.screen, open_car_menu s i, nobaction
            | `DeleteCar (i, j) ->
                let b_action = Backend.Action.RemoveStopCar{train=v.train; stop=i; car=j} in
                v.screen, None, b_action
            | `None ->
                v.screen, None, nobaction
            end

        | _ -> v.screen, None, nobaction
      in
      let v =
        if menu =!= v.menu || screen =!= v.screen || car_menu =!= v.car_menu then
          {v with menu; screen; car_menu} else v
      in
      let exit = Event.pressed_esc event in
      exit, v, bk_action

let handle_tick v time =
  begin match v.screen with
  | StationMap state -> Station_map_ui.handle_tick state time
  | _ -> ()
  end;
  v

