open Containers

module R = Renderer

(* Choose engine screen *)

let render win (s:State.t) ~region ~year =
  let engines = Engine.get region
    |> List.filter (fun engine -> engine.Engine.year <= 1880)
  in
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
      y + 25)
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
      y + 25)
    4
    engines
  in
  ()

    




