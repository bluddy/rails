open Containers
module R = Renderer

(* Station view screen *)

let render win (s:State.t) x y =
  let ground_y = 186 in
  let switchingyard_x = 0 in
  let engineshop_x = 64 in
  let station_x = 111 in
  let hotel_x = 207 in
  let restaurant_x = 242 in
  let storage_x = 285 in

  let hash =
    if Region.is_us s.backend.region then s.textures.station_us
    else s.textures.station_en
  in
  let get_tex x =
    let tex = Hashtbl.find hash x in
    let w = R.Texture.get_w tex in
    let h = R.Texture.get_h tex in
    tex, w, h
  in

  (* Background *)
  let win_h, win_w = R.height win, R.width win in
  R.draw_rect win ~fill:true ~x:0 ~y:0 ~w:win_w ~h:win_h ~color:Ega.cyan;
  let tex, _, h = get_tex `Background in
  R.Texture.render ~x:0 ~y:(win_h-h) win tex;

  let draw_rest_bottom () =
    let tex, _, h = get_tex `Rest_bottom in
    R.Texture.render ~x:restaurant_x ~y:(ground_y-h) win tex;
    h
  in
  let draw_goods_bottom () =
    let tex, _, h = get_tex `Goods_bottom in
    R.Texture.render ~x:storage_x ~y:(ground_y-h) win tex;
    h
  in
  let draw_smokestacks y =
    let tex, _, h = get_tex `Smokestacks in
    R.Texture.render ~x:storage_x ~y:(y-h) win tex
  in
  let draw_post_top y =
    let tex, _, h = get_tex `Post_top in
    R.Texture.render ~x:restaurant_x ~y:(y-h) win tex
  in

  let station = Backend.get_station s.backend x y |> Option.get_exn_or "station" in

  (* Draw name and year *)
  let font = Fonts.get_font s.fonts 4 in
  let name_s = Printf.sprintf "%s (%s)\nBuilt in %d" station.name (Station.kind_str station) station.year in
  Fonts.Font.write win font ~x:96 ~y:16 ~color:Ega.white name_s;

  (* draw upgrades *)
  Station.Upgrades.iter (function
    | EngineShop ->
        let tex, _, h = get_tex `EngineShop in
        R.Texture.render ~x:engineshop_x ~y:(ground_y-h) win tex
    | SwitchingYard ->
        let tex, _, h = get_tex `SwitchingYard in
        R.Texture.render ~x:switchingyard_x ~y:(ground_y-h) win tex
    | MaintenanceShop ->
        let tex, _, h = get_tex `Barn in
        R.Texture.render ~x:engineshop_x ~y:(ground_y-h) win tex
    | ColdStorage when Station.Upgrades.mem (Station.get_upgrades station) GoodsStorage ->
        let h_bottom = draw_goods_bottom () in
        let tex, _, h = get_tex `Goods in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:storage_x ~y win tex;
        let tex, _, h = get_tex `Cold in
        let y = y - h in
        R.Texture.render ~x:storage_x ~y win tex;
        draw_smokestacks y
    | ColdStorage ->
        let h_bottom = draw_goods_bottom () in
        let tex, _, h = get_tex `Cold in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:storage_x ~y win tex;
        draw_smokestacks y
    | GoodsStorage when not @@ Station.Upgrades.mem (Station.get_upgrades station) ColdStorage ->
        (* Only goods *)
        let h_bottom = draw_goods_bottom () in
        let tex, _, h = get_tex `Goods in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:storage_x ~y win tex;
        draw_smokestacks y
    | LivestockPens ->
        let tex, _, h = get_tex `Fence in
        R.Texture.render ~x:storage_x ~y:(ground_y-h) win tex
    | PostOffice when Station.Upgrades.mem (Station.get_upgrades station) Restaurant ->
        (* both post office and resaurant *)
        let h_bottom = draw_rest_bottom () in
        let tex, _, h = get_tex `Restaurant in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:restaurant_x ~y win tex;
        let tex, _, h = get_tex `PostOffice in
        let y = y - h in
        R.Texture.render ~x:restaurant_x ~y win tex;
        draw_post_top y
    | PostOffice ->
        (* Only post office *)
        let h_bottom = draw_rest_bottom () in
        let tex, _, h = get_tex `PostOffice in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:restaurant_x ~y win tex;
        draw_post_top y
    | Restaurant when not (Station.Upgrades.mem (Station.get_upgrades station) PostOffice) ->
        (* Only restaurant *)
        let h_bottom = draw_rest_bottom () in
        let tex, _, h = get_tex `Restaurant in
        let y = ground_y - h_bottom - h in
        R.Texture.render ~x:restaurant_x ~y win tex;
        draw_post_top y
    | Hotel ->
        let tex, _, h = get_tex `Hotel in
        R.Texture.render ~x:hotel_x ~y:(ground_y-h) win tex
    | _ -> ()
  )
  (Station.get_upgrades station);

  (* Draw Station *)
  let station_kind :> Textures.Station.hash = match station.info with
    | Some {kind;_} -> kind
    | None -> assert false
  in
  let tex, _, h = get_tex station_kind in
  R.Texture.render ~x:station_x ~y:(ground_y-h) win tex;
  ()

