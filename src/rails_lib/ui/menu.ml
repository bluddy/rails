include Engine.Menu

module MsgBox = struct
  include Engine.Menu.MsgBox

  let make ?heading ?x ?y ?font_idx ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries =
    make ?heading ?x ?y ?font_idx:(Option.map Fonts.face_to_enum font_idx) ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries

  let make_basic ?x ?y ?wh ?heading ?tight ?font_idx ~fonts s text =
    make_basic ?x ?y ?wh ?heading ?tight ?font_idx:(Option.map Fonts.face_to_enum font_idx) ~fonts s text
end

module Global = struct
  include Engine.Menu.Global

  let make fonts ?font_idx menus ~w ~h =
    make fonts ?font_idx:(Option.map Fonts.face_to_enum font_idx) menus ~w ~h
end

module Animated = struct
  include Engine.Menu.Animated

  let make_global fonts ?font_idx menus ~w ~h =
    make_global fonts ?font_idx:(Option.map Fonts.face_to_enum font_idx) menus ~w ~h

  let make_msgbox ?heading ?x ?y ?font_idx ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries =
    make_msgbox ?heading ?x ?y ?font_idx:(Option.map Fonts.face_to_enum font_idx) ?select_color ?draw_bg ?use_prefix ?border_x ?border_y ~fonts entries
end
