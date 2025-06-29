open Containers

module R = Renderer

type t = {
  cursor: int option;
  text: string;
  x: int;
  y: int;
  num_chars: int;
}

let char_width = 8
let char_height = 8

let make ?(editable=true) text ~x ~y ~chars =
  let cursor = if editable then Some 0 else None in
  let len = String.length text in
  let num_spaces = max 0 (chars - len) in
  let text = text ^ String.make num_spaces ' ' in
  {
    cursor; text; x; y; num_chars=chars;
  }

let get_text v = String.rdrop_while (function ' ' -> true | _ -> false) v.text

let render win fonts v =
  (* draw frame *)
  let w = v.num_chars * char_width + 2 + 2 in
  let h = 8 + 2 + 3 in
  R.draw_rect win ~x:v.x ~y:v.y ~w ~h ~color:Ega.black ~fill:false;

  (* draw text *)
  let y = v.y + 2 in
  let x = v.x + 2 in
  match v.cursor with
  | None ->
    Fonts.Render.write win fonts v.text ~color:Ega.black ~idx:`Standard ~x ~y
  | Some cursor ->
    Fonts.Render.write win fonts v.text ~cursor:(cursor, Ega.bcyan) ~color:Ega.black ~idx:`Standard ~x ~y

let handle_event v event =
  match v.cursor with
  | None -> v, `Stay
  | Some cursor ->
    begin match event with
    | Event.Key ({down=true; key; _} as keyinfo) ->
      let insert_char c v =
        let text1, text2 = String.take cursor v.text, String.drop (cursor + 1) v.text in
        Printf.sprintf "%s%c%s" text1 c text2
      in
      begin match key with
      | _ when Event.is_alphanumeric key && cursor < v.num_chars ->
        let shift = Event.Modifiers.shift keyinfo.modifiers in
        let c = Event.char_of_key ~shift key in
        let text = insert_char c v in
        {v with text; cursor=Some(cursor + 1)}, `Stay
      | Space when cursor < v.num_chars ->
        let text = insert_char ' ' v in
        {v with text; cursor=Some(cursor + 1)}, `Stay
      | Left when cursor > 0 ->
        {v with cursor=Some(cursor - 1)}, `Stay
      | Right when cursor < String.length v.text - 1 ->
        {v with cursor=Some(cursor + 1)}, `Stay
      | Backspace when cursor > 0 ->
        let text1, text2 = String.take (cursor-1) v.text, String.drop cursor v.text in
        let text = text1 ^ text2 ^ " " in
        {v with cursor=Some(cursor-1); text}, `Stay
      | Enter -> v, `Return(get_text v)
      | _ -> v, `Stay
      end
    | _ -> v, `Stay
    end

let set_writeable x v = match x, v.cursor with
  | true, None -> {v with cursor=Some 0}
  | false, Some _ -> {v with cursor=None}
  | _ -> v

