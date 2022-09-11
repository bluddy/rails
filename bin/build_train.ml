open Containers
open Utils.Infix

module R = Renderer

module AddCars = struct
  open Build_train_d

  (* Create the animation that will be used when we add cars *)
  let init (s:State.t) ~engine =
    (* Find station with engine shop *)
    let station =
      try
        Station_map.filter s.backend.stations
        (fun station -> Station.has_upgrade station ~upgrade:Station.EngineShop)
        |> Iter.head_exn
      with Invalid_argument _ -> invalid_arg "No station with engine found"
    in
    let station_x, station_y = station.x, station.y in
    let anim =
      let engine = engine.Engine.make in
      Train_animate_side.init s ~engine ~cars:[] ~paused:false ~station_x ~station_y ~rail:`Back
    in
    let menu =
      let goods = Goods.of_region s.backend.region in
      let car_list = List.map Goods.car_str_of goods in
      let goods_cars = List.combine car_list goods in
      let open Menu.MsgBox in
      make ~fonts:s.fonts ~heading:"Add Car?" @@
        [make_entry "No Thanks" (`Action `Done)] @
        (List.map (fun (name, good) -> make_entry name @@ `Action(`AddCar good)) goods_cars)
    in
    {
      anim;
      menu;
      show_menu=false;
    }

  let handle_event (_s:State.t) _event v = v

  let handle_tick s v time =
    let anim = Train_animate_side.handle_tick s v.anim time in
    (* Check if we reached end *)
    let anim =
      if Train_animate_side.train_end_at_screen_edge s anim then
        Train_animate_side.pause anim
      else anim
    in
    if anim =!= v.anim then {v with anim} else v

  let render win (s:State.t) v =
    Train_animate_side.render win s v.anim;
    if v.show_menu then
      Menu.MsgBox.render win s v.menu;
    ()

end

module ChooseEngine = struct
  (* Choose engine screen: select with mouse only *)
  let engine_start_y = 24
  let engine_each_y = 25

  let get_engines_before region year =
    Engine.get region
    |> List.filter (fun engine -> engine.Engine.year <= year)

  let render win (s:State.t) ~region ~year =
    let engines = get_engines_before region year in
    let engine_anims =
      List.map (fun engine ->
        Hashtbl.find s.textures.Textures.engine_anim engine.Engine.make)
        engines
    in
    let track_tex = Hashtbl.find s.textures.Textures.misc `SideTrack in
    R.paint_screen win ~color:Ega.bcyan;
    let _ =
      let open Textures.TrainAnim in
      List.fold_left (fun y anim ->
        let h = R.Texture.get_h anim.tex in
        R.Texture.render win anim.tex ~x:8 ~y:(21-h+y+1);
        R.Texture.render win track_tex ~x:0 ~y:(22+y);
        y + engine_each_y)
      0
      engine_anims
    in
    (* Draw frame *)
    R.draw_rect win ~x:160 ~y:0 ~w:160 ~h:200 ~color:Ega.white ~fill:true;
    R.draw_rect win ~x:160 ~y:2 ~w:158 ~h:196 ~color:Ega.black ~fill:false;

    (* Write details *)
    let font = Fonts.get_font s.fonts 4 in
    let _ =
      List.fold_left (fun y engine ->
        let price = Utils.string_of_num engine.Engine.price in
        let str = Printf.sprintf "%s\n%d mph %dhp $%s"
          engine.name engine.max_speed engine.horsepower price
        in
        Fonts.Font.write win font ~color:Ega.cyan str ~x:164 ~y;
        y + engine_each_y)
      4
      engines
    in
    ()

  let handle_event (event:Event.t) ~region ~year =
    match event with
    | MouseButton {y; _} when Event.is_left_click event ->
      let engines = get_engines_before region year in
      if y <= 24 then List.nth engines 0 |> Option.some else
        let click_idx = ((y - engine_start_y) / engine_each_y) + 1 in
        if click_idx >= List.length engines then None
        else List.nth engines click_idx |> Option.some
    | _ -> None

end

