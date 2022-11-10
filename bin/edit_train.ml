open Containers
module R = Renderer
module B = Backend

(* The edit train screen *)

type ('msg, 'state) t = {
  index: int;
  menu: ('msg, 'state) Menu.Global.t;
}

let nobaction = B.Action.NoAction

let make_menu fonts menu_h =
  let open Menu in
  let engine_menu = MsgBox.make ~fonts ~x:56 ~y:8 [] in
  let train_type_menu = MsgBox.make ~fonts ~x:56 ~y:8 [] in
  let route_map = MsgBox.make ~fonts ~x:56 ~y:8 [] in

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

let render win _s _v =
  (* Draw screen background *)
  R.paint_screen win ~color:Ega.white;
  R.draw_rect win ~x:2 ~y:9 ~color:Ega.black ~w:315 ~h:106 ~fill:false;
  R.draw_line win ~color:Ega.black ~x1:2 ~y1:49 ~x2:316 ~y2:49;
  R.draw_line win ~color:Ega.black ~x1:0 ~y1:115 ~x2:319 ~y2:115;
  R.draw_line win ~color:Ega.black ~x1:0 ~y1:116 ~x2:319 ~y2:116;
  ()

let handle_event _s v event =
  if Event.pressed_esc event then
    true, v, nobaction
  else
    false, v, nobaction
