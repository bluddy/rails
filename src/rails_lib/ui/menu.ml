include Engine.Menu

let padding = {
  top=5;
  bottom=5;
  left=5;
  right=7;
  entry_spacing=3;
}

let colors = {
  select=Ega.bcyan;
  bg=Ega.gray;
  entry=Ega.black;
  heading=Ega.white;
  outer_border=Ega.black;
  inner_border=Some(Ega.white)
}

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
