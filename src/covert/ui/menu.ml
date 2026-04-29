include Engine.Menu

module Animated = struct
  include Engine.Menu.Animated

  type nonrec 'msg t = ('msg, unit) t

  let make_msgbox ?heading ?x ?y ?font_idx ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries : 'msg t =
    make_msgbox ?heading ?x ?y ?font_idx ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries

  let render win v = render win () v

  let handle_event v event time : 'msg t = handle_event () v event time |> fst

  let handle_tick v time : 'msg t * 'msg action = handle_tick () v time

end

include Animated

let static_entry = MsgBox.static_entry

let make_entry = MsgBox.make_entry

let do_open_menu = MsgBox.do_open_menu

