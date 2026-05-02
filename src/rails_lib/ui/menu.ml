include Engine.Menu
module Ega = Engine.Ega

let padding = {
  top=5;
  bottom=5;
  left=5;
  right=7;
  entry_spacing=0;
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

  let g_padding = padding

  let make ?heading ?x ?y ?(font_idx=`Caps) ?draw_bg ?indent_entries ?padding ?select_color ~fonts entries =
    let font_idx = Fonts.face_to_enum font_idx in
    let colors = match select_color with
    | Some color -> {colors with select=color}
    | _ -> colors
    in
    let padding = match padding with
    | Some padding -> padding
    | _ -> g_padding
    in
    make ?heading ?x ?y ~font_idx ?draw_bg ~padding ~colors ~fonts ?indent_entries entries


  let make_basic ?x ?y ?wh ?heading ?tight ?(font_idx=`Standard) ~fonts s text =
    let font_idx = Fonts.face_to_enum font_idx in
    make_basic ?x ?y ?wh ?heading ?tight ~font_idx ~padding ~colors ~fonts s text
end

module Global = struct
  include Engine.Menu.Global

  let make fonts ?(font_idx=`Caps) menus ~w ~h =
    let font_idx = Fonts.face_to_enum font_idx in
    make fonts ~font_idx menus ~w ~h
end

module Animated = struct
  include Engine.Menu.Animated

  let make_global fonts ?(font_idx=`Caps) menus ~w ~h =
    let font_idx = Fonts.face_to_enum font_idx in
    make_global fonts ~font_idx menus ~w ~h

  let make_msgbox ?heading ?x ?y ?(font_idx=`Caps) ?draw_bg ?indent_entries ~fonts entries =
    let font_idx = Fonts.face_to_enum font_idx in
    make_msgbox ?heading ?x ?y ~font_idx ?draw_bg ?indent_entries ~padding ~colors ~fonts entries
end
