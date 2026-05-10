include Engine.Fonts

type face = [
  | `Old
  | `Caps
  | `Large
] [@@deriving enum]

let get_font idx v = get_font (face_to_enum idx) v

module Render = struct
  include Engine.Fonts.Render

  let write_char win fonts ~color ~idx c ~x ~y =
    write_char win fonts ~color ~idx:(face_to_enum idx) c ~x ~y

  let write win fonts ?active_color ?tag_color ?cursor ?tight ~color ~idx str ~x ~y =
    write win fonts ?active_color ?tag_color ?cursor ?tight ~color ~idx:(face_to_enum idx) str ~x ~y

  let write_shadow win fonts ~color ~idx str ~x ~y =
    write_shadow win fonts ~color ~idx:(face_to_enum idx) str ~x ~y
end

let write_str ?color idx str ~fonts ~x ~y =
  write_str ?color (face_to_enum idx) str ~fonts ~x ~y
