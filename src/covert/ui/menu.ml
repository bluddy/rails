include Engine.Menu
module Ega = Engine.Ega

(* All msgboxes are animated. No global menu *)

let padding = {
  top=5;
  bottom=3;
  left=5;
  right=5;
  entry_spacing=2;
}

let colors = {
  select=Ega.yellow;
  bg=Ega.black;
  entry=Ega.gray;
  heading=Ega.white;
  outer_border=Ega.gray;
  inner_border=None;
}

module Animated = struct
  include Engine.Menu.Animated

  type nonrec 'msg t = ('msg, unit) t

  let make_msgbox ?heading ?x ?y ?(font_idx=`Large) ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries : 'msg t =
    make_msgbox ?heading ?x ?y ~font_idx:(Fonts.face_to_enum font_idx) ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries

  let render win v = render win () v

  let handle_event v event time : 'msg t = handle_event () v event time |> fst

  let handle_tick v time : 'msg t * 'msg action = handle_tick () v time

  let do_open_menu ?x ?y ?wh ?selected (v:'msg t) : 'msg t=
    do_open_menu ?x ?y ?wh ?selected () v

  let modal_handle_event ?is_msgbox v event time =
    modal_handle_event ?is_msgbox () v event time

  let modal_handle_tick v time =
    modal_handle_tick () v time
end

include Animated


