open Containers
module Hashtbl = Utils.Hashtbl
module R = Renderer

type t = {
  engine: Engine.t;
}

let make engine = 
  { engine }

let render win v ~fonts ~textures ~region =
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:2 ~y:2 ~w:315 ~h:195 ~color:Ega.dgray ~fill:false;
  (* center name *)
  let x = 160 - 7 * String.length v.engine.name in
  Fonts.Render.write win fonts ~color:Ega.black ~idx:2 v.engine.name ~x ~y:4;
  let write = Fonts.Render.write win fonts ~idx:4 ~color:Ega.black in
  let tex = Hashtbl.find textures.Textures.engine_detail v.engine.make in
  R.Texture.render win tex ~x:0 ~y:21;

  let open Printf in
  let spd_str = sprintf "Maximum Speed: %d mph." (v.engine.max_speed * 5) in
  let hp_str = sprintf "Power at Drawbar: %dhp" (v.engine.horsepower * 500) in
  let price = sprintf "Price: %s" (Money.print ~region v.engine.price) in
  let str = sprintf "%s\n%s\n%s" spd_str hp_str price in
  write ~x:80 ~y:88 str;
  write ~x:80 ~y:120 "Rated Train Speed / %Grade:";

  let rec loop cars y =
    let weight = cars * 160 * 2 in
    write ~x:6 ~y @@ sprintf "%d cars:" cars;
    let _, speed =
      Iter.fold (fun (x, _) grade ->
        let grade = grade * 2 in
        let a = weight/160 + 1 in
        let b = a * (grade + 2) in
        let c = 200 * v.engine.horsepower / b in
        let speed = (c * 8 + 80) / 16
         |> Utils.clip ~min:0 ~max:(v.engine.max_speed * 5)
        in
        let grade_clip = Utils.clip ~min:0 ~max:3 grade in
        write ~x ~y @@ sprintf "%d mph (%d%%)," speed grade_clip;
        (x+80, speed)
      )
      (79, 0)
      Iter.(0--2)
    in
    if cars < 3 || (speed > 10 && cars < 8) then
      loop (cars + 1) (y + 8)
    else ()
  in
  loop 1 131;
  ()

let handle_event (event:Event.t) = match event with
  | Key {down=true;_}
  | MouseButton {down=true;_}-> `Exit
  | _ -> `NoEvent


