open Containers
module R = Renderer
module B = Backend
open Edit_train_d

(* The edit train screen *)

let nobaction = B.Action.NoAction

let make_menu fonts menu_h =
  let open Menu in
  let engine_menu =
    let open MsgBox in
    make ~fonts [
      make_entry "Dummy" @@ `Action `Dummy
    ]
    in
  let train_type_menu = engine_menu in
  let route_map = engine_menu in

  let titles =
    let open Menu.Title in
    [
      make ~fonts ~x:8 ~y:1 "&Engine" engine_menu;
      make ~fonts ~x:72 ~y:1 "&Train type" train_type_menu;
      make ~fonts ~x:160 ~y:1 "&Route map" route_map;
    ]
  in
  Menu.Global.make ~menu_h titles

let make ~fonts index =
  let menu = make_menu fonts 8 in
  {
    index;
    menu;
  }

let render win (s:State.t) v =
  let train = Backend.get_train s.backend v.index in
  let write color = Fonts.Render.write win s.fonts ~color ~idx:4 in

  (* Draw screen background *)
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:2 ~y:9 ~color:Ega.black ~w:316 ~h:107 ~fill:false;
  R.draw_line win ~color:Ega.black ~x1:2 ~y1:49 ~x2:316 ~y2:49;
  R.draw_line win ~color:Ega.black ~x1:0 ~y1:115 ~x2:319 ~y2:115;
  R.draw_line win ~color:Ega.black ~x1:0 ~y1:116 ~x2:319 ~y2:116;

  (* Menu bar *)
  Menu.Global.render win s s.fonts v.menu ~w:s.ui.dims.screen.w ~h:8;

  (* TODO: make these things dynamic *)
  let open Printf in
  let line1 = sprintf "Train #%d: %s %s\n" v.index (Goods.show_freight train.freight) "Limited" in
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

  (* Write stops *)
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
  ()

let handle_event _s v event =
  if Event.pressed_esc event then
    true, v, nobaction
  else
    false, v, nobaction

