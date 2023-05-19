open Containers
module Hashtbl = Utils.Hashtbl
module R = Renderer

type t = {
  engine: Engine.t;
}

let make engine = 
  { engine }

let render win v ~fonts ~textures =
  R.paint_screen win ~color:Ega.white;
  Fonts.Render.write win fonts ~color:Ega.black ~idx:2 v.engine.name ~x:71 ~y:4;
  let tex = Hashtbl.find textures.Textures.engine_detail v.engine.make in
  R.Texture.render win tex ~x:0 ~y:21;
  ()

let handle_event (event:Event.t) = match event with
  | Key {down=true;_}
  | MouseButton {down=true;_}-> `Exit
  | _ -> `NoEvent


