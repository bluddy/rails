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
  R.paint_screen win ~color:Ega.bcyan;
  let _ =
    List.fold_left (fun y anim ->
      R.Texture.render win anim.Textures.TrainAnim.tex ~x:8 ~y;
      y + 25)
    3
    engine_anims
  in
  ()

    




