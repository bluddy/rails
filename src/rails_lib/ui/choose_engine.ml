open Containers
module R = Renderer

(* Choose engine screen: select with mouse only *)
let engine_start_y = 24
let engine_each_y = 25

let get_relevant_engines engines year =
  List.filter (fun engine ->
      engine.Engine.year <= year && year - engine.year <= 50)
    engines

let render win (s:State.t) ~engines ~year =
  let engines = Engine.available_at_year engines ~year in
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
      let price = Utils.string_of_num (engine.Engine.price * 1000) in
      let str = Printf.sprintf "%s\n%d mph %dhp $%s"
        engine.name (engine.max_speed * 5) (engine.horsepower * 500) price
      in
      Fonts.Font.write win font ~color:Ega.cyan str ~x:164 ~y;
      y + engine_each_y)
    4
    engines
  in
  ()

let handle_event (event:Event.t) engines ~year =
  match event with
  | MouseButton {y; _} when Event.is_left_click event ->
    let engines = get_relevant_engines engines year in
    if y <= 24 then List.nth engines 0 |> Option.some else
      let click_idx = ((y - engine_start_y) / engine_each_y) + 1 in
      if click_idx >= List.length engines then None
      else List.nth engines click_idx |> Option.some
  | _ -> None

