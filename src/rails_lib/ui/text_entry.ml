open Containers

module R = Renderer

type t = {
  cursor: int;
  text: string;
  x: int;
  y: int;
  num_chars: int;
}

let char_width = 8
let char_height = 8

let make text x y ~chars =
  {
    cursor=0; text; x; y; num_chars=chars;
  }

let render win fonts v =
  (* draw frame *)
  let w = v.num_chars * char_width + 2 + 2 in
  let h = 8 + 2 + 3 in
  R.draw_rect win ~x:v.x ~y:v.y ~w ~h ~color:Ega.black ~fill:false;

  (* draw cursor *)
  let y = v.y + 1 in
  let x = v.x + 2 + char_width * v.cursor in
  let w, h = char_width, char_height in
  R.draw_rect win ~x ~y ~w ~h ~color:Ega.bcyan ~fill:true;

  (* draw text *)
  let y = v.y + 1 in
  let x = v.x + 2 in
  Fonts.Render.write win fonts v.text ~color:Ega.black ~idx:4 ~x ~y

let handle_event v = function
  | Event.Key ({down=true; key; _} as keyinfo) ->
    let insert_char c v =
      let text1, text2 = String.take v.cursor v.text, String.drop (v.cursor + 1) v.text in
      Printf.sprintf "%s%c%s" text1 c text2
    in
    begin match key with
    | _ when Event.is_alphanumeric key ->
      let c = Event.char_of_key key in
      let c =
        if Event.Modifiers.shift keyinfo.modifiers then
          Char.uppercase_ascii c
        else
          Char.lowercase_ascii c
      in
      let text = insert_char c v in
      {v with text; cursor=v.cursor + 1}, `Stay
    | Space ->
      let text = insert_char ' ' v in
      {v with text; cursor=v.cursor + 1}, `Stay
    | Left when v.cursor > 0 ->
      {v with cursor=v.cursor - 1}, `Stay
    | Right when v.cursor < String.length v.text ->
      {v with cursor=v.cursor + 1}, `Stay
    | Backspace when v.cursor > 0 ->
      let text1, text2 = String.take v.cursor v.text, String.drop (v.cursor + 1) v.text in
      let text = text1 ^ text2 in
      {v with cursor=v.cursor-1; text}, `Stay
    | Enter -> v, `Return v.text
    | _ -> v, `Stay
    end
  | _ -> v, `Stay

