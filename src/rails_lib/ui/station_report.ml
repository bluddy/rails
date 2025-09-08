open Containers
module R = Renderer
module C = Constants
module B = Backend

(* Station view screen *)

let get_tex (s:State.t) x =
  let hash =
    if Region.is_us (B.get_region s.backend) then s.textures.station_us
    else s.textures.station_en
  in
  let tex = Hashtbl.find hash x in
  let w = R.Texture.get_w tex in
  let h = R.Texture.get_h tex in
  tex, w, h

let render_background (s:State.t) win =
  (* Background *)
  let win_h, win_w = R.height win, R.width win in
  R.draw_rect win ~fill:true ~x:0 ~y:0 ~w:win_w ~h:win_h ~color:Ega.cyan;
  let tex, _, h = get_tex s `Background in
  R.Texture.render ~x:0 ~y:(win_h-h) win tex

let render_foreground ?(show_name=true) win (s:State.t) ?station loc ~show_demand =
  let win_h, win_w = R.height win, R.width win in
  let engineshop_x = 64 in
  let ground_y = 186 in
  let switchingyard_x = 0 in
  let station_x = 111 in
  let hotel_x = 207 in
  let restaurant_x = 242 in
  let storage_x = 285 in

  let draw_rest_bottom () =
    let tex, _, h = get_tex s `Rest_bottom in
    R.Texture.render ~x:restaurant_x ~y:(ground_y-h) win tex;
    h
  in
  let draw_goods_bottom () =
    let tex, _, h = get_tex s `Goods_bottom in
    R.Texture.render ~x:storage_x ~y:(ground_y-h) win tex;
    h
  in
  let draw_smokestacks y =
    let tex, _, h = get_tex s `Smokestacks in
    R.Texture.render ~x:storage_x ~y:(y-h) win tex
  in
  let draw_post_top y =
    let tex, _, h = get_tex s `Post_top in
    R.Texture.render ~x:restaurant_x ~y:(y-h) win tex
  in

  let station = match station with
  | Some station -> station
  | _ -> Backend.get_station loc s.backend |> Option.get_exn_or "station" in
  let info = Option.get_exn_or "Not a real station" station.info in

  let goods_and_other () =
    let h_bottom = draw_goods_bottom () in
    let tex, _, h = get_tex s `Goods in
    let y = ground_y - h_bottom - h in
    R.Texture.render ~x:storage_x ~y win tex;
    let tex, _, h = get_tex s `Cold in
    let y = y - h in
    R.Texture.render ~x:storage_x ~y win tex;
    draw_smokestacks y
  in

  (* draw upgrades *)
  Station.Upgrades.iter (function
    | EngineShop ->
        let tex, _, h = get_tex s `EngineShop in
        R.Texture.render ~x:engineshop_x ~y:(ground_y-h) win tex
    | SwitchingYard ->
        let tex, _, h = get_tex s `SwitchingYard in
        R.Texture.render ~x:switchingyard_x ~y:(ground_y-h) win tex
    | MaintenanceShop ->
        let tex, _, h = get_tex s `Barn in
        R.Texture.render ~x:engineshop_x ~y:(ground_y-h) win tex
    | ColdStorage when Station.Upgrades.mem (Station.get_upgrades station) GoodsStorage ->
        goods_and_other ()
    | ColdStorage when Station.Upgrades.mem (Station.get_upgrades station) ArmsStorage ->
        goods_and_other ()
    | ColdStorage ->
        let h_bottom = draw_goods_bottom () in
        let tex, _, h = get_tex s `Cold in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:storage_x ~y win tex;
        draw_smokestacks y
    | (GoodsStorage | ArmsStorage) when not @@ Station.Upgrades.mem (Station.get_upgrades station) ColdStorage ->
        (* Only goods *)
        let h_bottom = draw_goods_bottom () in
        let tex, _, h = get_tex s `Goods in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:storage_x ~y win tex;
        draw_smokestacks y
    | LivestockPens | GrapeStorage ->
        let tex, _, h = get_tex s `Fence in
        R.Texture.render ~x:storage_x ~y:(ground_y-h) win tex
    | PostOffice when Station.Upgrades.mem (Station.get_upgrades station) Restaurant ->
        (* both post office and resaurant *)
        let h_bottom = draw_rest_bottom () in
        let tex, _, h = get_tex s `Restaurant in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:restaurant_x ~y win tex;
        let tex, _, h = get_tex s `PostOffice in
        let y = y - h in
        R.Texture.render ~x:restaurant_x ~y win tex;
        draw_post_top y
    | PostOffice ->
        (* Only post office *)
        let h_bottom = draw_rest_bottom () in
        let tex, _, h = get_tex s `PostOffice in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:restaurant_x ~y win tex;
        draw_post_top y
    | Restaurant when not (Station.Upgrades.mem (Station.get_upgrades station) PostOffice) ->
        (* Only restaurant *)
        let h_bottom = draw_rest_bottom () in
        let tex, _, h = get_tex s `Restaurant in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:restaurant_x ~y win tex;
        draw_post_top y
    | Hotel ->
        let tex, _, h = get_tex s `Hotel in
        R.Texture.render ~x:hotel_x ~y:(ground_y-h) win tex
    | _ -> ()
  )
  (Station.get_upgrades station);

  (* Draw Station *)
  let station_kind :> Textures.StationTex.hash = match station.info with
    | Some {kind;_} -> kind
    | None -> assert false
  in
  let tex, _, h = get_tex s station_kind in
  R.Texture.render ~x:station_x ~y:(ground_y-h) win tex;

  let font = Fonts.get_font `Standard s.fonts in

  let write_name ~shadow ~x ~y ~color =
    let str = Printf.sprintf "%s (%s)\nBuilt in %d" (Station.get_name station) (Station.kind_str station) station.year in
    if shadow then
      Fonts.Render.write_shadow win s.fonts ~color ~x ~y ~idx:`Standard str
    else
      Fonts.Font.write win font ~x ~y ~color str
  in

  if show_demand then (
    (* draw demand of goods background *)
    R.draw_rect win ~x:0 ~y:0 ~w:win_w ~h:100 ~color:Ega.white ~fill:true;
    R.draw_line win ~x1:0 ~y1:(win_h/2) ~x2:win_w ~y2:(win_h/2) ~color:Ega.black;
    R.draw_line win ~x1:(win_w/2) ~y1:0 ~x2:(win_w/2) ~y2:(win_h/2) ~color:Ega.black;

    (* Supply side *)
    Fonts.Font.write win font ~x:32 ~y:1 ~color:Ega.black "Waiting for pickup...";
    let _ =
      List.fold_left (fun y good ->
        match Hashtbl.get info.supply good with
        | Some amount ->
            Fonts.Font.write win font ~x:2 ~y:(y+1) ~color:Ega.black (Goods.show good);
            let tex = Hashtbl.find s.textures.route_cars @@ `CarOld good in
            let tex_w = R.Texture.get_w tex in
            (* Draw (partial) cars of supply *)
            let rec loop x amount =
              if amount >= C.car_amount then (
                R.Texture.render win ~x ~y tex;
                loop (x + tex_w) (amount - C.car_amount)
              ) else (
                let frac = (float_of_int amount) /. (float_of_int C.car_amount) in
                let w_frac = int_of_float @@ frac *. (float_of_int tex_w) in
                R.Texture.render_subtex win tex ~x ~y ~w:w_frac
              )
            in
            loop 64 amount;
            y + 10
        | _ -> y)
      9
      Goods.order
    in
    (* Demand side *)
    let x = 192 in
    Fonts.Font.write win font ~x ~y:1 ~color:Ega.black "Will pay for...";
    (* TODO: transition to new cars with year X *)
    let _ =
      Goods.Set.fold (fun good y ->
        Fonts.Font.write win font ~x ~y ~color:Ega.black (Goods.show good);
        let tex = Hashtbl.find s.textures.route_cars @@ `CarOld good in
        R.Texture.render win ~x:162 ~y tex;
        y + 10)
      info.demand
      10
    in
    (* Priority shipment *)
    let goods_s = Backend.get_player C.player s.backend 
      |> Player.get_priority
      |> Priority_shipment.station_waiting_delivery_text ~loc
    in
    Option.iter
      (fun goods ->
        let str = "PRIORITY\nDELIVERY:\n" ^ goods in
        Fonts.Render.write_shadow win s.fonts ~color:Ega.white ~idx:`Standard ~x:210 ~y:104 str)
      goods_s;
  );

  if show_name then (
    (* name with shadow *)
    write_name ~shadow:true ~x:8 ~y:104 ~color:Ega.white
  )

let render ?(show_name=true) win (s:State.t) ?station loc ~show_demand =
  render_background s win;
  render_foreground ~show_name win s ?station loc ~show_demand

