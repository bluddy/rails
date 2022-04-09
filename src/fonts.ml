open Containers
open Iter.Infix

module Font = struct

type t =
  {
    char_byte_length: int;
    space_x: int;
    space_y: int;
    char_widths: (char, int) Hashtbl.t;
    characters: (char, string) Hashtbl.t;
    height: int;
    textures: (char, Renderer.Texture.t) Hashtbl.t; [@opaque]
  } [@@ deriving show]

let get_str font c =
  Hashtbl.find font.characters c

let get_width font c =
  Hashtbl.find font.char_widths c

module Ndarray = Owl_base_dense_ndarray.Generic

let write_letter ?(x=0) ?(y=0) font ~pixels c =
  (* create an ARGB array with the letter *)
  let x_off, y_off = x, y in
  let width = Hashtbl.find font.char_widths c in
  let char_str = Hashtbl.find font.characters c in
  let byte, bit = ref 0, ref 0 in
  for y=0 to font.height - 1 do
    if !bit > 0 then begin
      bit := 0;
      incr byte;
    end;
    for x=0 to font.char_byte_length * 8 - 1 do
      if x < width then begin
        let color =
          if ((Char.to_int char_str.[!byte]) land (0x80 lsr !bit)) > 0 then 0xFF else 0
        in
        for i=0 to 3 do
          (* Printf.printf "y_off:%d, y:%d, x_off:%d, x:%d, i:%d" y_off y x_off x i; *)
          Ndarray.set pixels [|y_off + y;x_off + x; i|] color
        done
      end;
      incr bit;
      if !bit = 8 then begin
        bit := 0;
        incr byte;
      end
    done;
  done

let get_letter font c =
  let width = Hashtbl.find font.char_widths c in
  let pixels = Ndarray.empty Int8_unsigned [|font.height; width; 4|] in
  ignore @@ write_letter font ~pixels c;
  pixels

let get_letter_width font c =
  Hashtbl.find font.char_widths c

  (* write to RGBA ndarray *)
let write ?(x=0) ?(y=0) ~font str ~pixels =
  let x_off, y_off = x, y in
  let x, y =
    String.fold (fun (x,y) c ->
      let y_down = y + font.height + font.space_y in
      if Char.equal c '\n' then
        (x_off, y_down)
      else
        let w = get_letter_width font c in
        let x, y =
          (* check if we fit on the line *)
          if x + w >= Ndarray.nth_dim pixels 1 then
            (x_off, y_down)
          else
            (x, y)
        in
        write_letter font ~pixels c ~x ~y;
        (x + w + font.space_x, y))
    (x_off, y_off)
    str
  in
  x, y

  module R = Renderer

  let create_textures win v =
    Hashtbl.keys v.characters (fun c ->
      let ndarray = get_letter v c in
      let tex = R.Texture.make win ndarray in
      Hashtbl.replace v.textures c tex
    )

end

(* fonts:
  0: fancy olde style
  1: all-caps
  2: large
  4: standard
*)

type t = Font.t array

let of_file filename : t =
  let bytes =
    IO.with_in filename @@
      fun in_channel -> IO.read_all_bytes in_channel
  in
  let index = 0 in
  let num_fonts = Bytes.get_uint16_le bytes index in
  let index = index + 2 in
  let indices = Iter.int_range_by index (index - 1 + num_fonts * 2) ~step:2 in
  let font_offsets = Iter.fold
    (fun acc offset ->
      let font_offset = Bytes.get_uint16_le bytes offset in
      font_offset::acc)
    []
    indices
  in
  let create_font bytes offset =
    let ascii_first = Bytes.get_uint8 bytes @@ offset - 8 in
    let ascii_last = Bytes.get_uint8 bytes @@ offset - 7 in
    let char_byte_length = Bytes.get_uint8 bytes @@ offset - 6 in
    let char_top_row = Bytes.get_uint8 bytes @@ offset - 5 in
    let char_bot_row = Bytes.get_uint8 bytes @@ offset - 4 in
    let space_x = Bytes.get_uint8 bytes @@ offset - 3 in
    let space_y = Bytes.get_uint8 bytes @@ offset - 2 in
    let char_count = ascii_last - ascii_first + 1 in
    let characters = Hashtbl.create 50 in
    let char_widths = Hashtbl.create 50 in
    let _ =
      Iter.foldi (fun index i c ->
        let c = char_of_int c in
        let i = i+1 in
        let b = Bytes.create @@ (1 + char_bot_row - char_top_row) * char_byte_length in
        for row = 0 to char_bot_row - char_top_row do
          for col = 0 to char_byte_length - 1 do
            let ind = row * char_byte_length + col in
            let bin = index + col + row * char_byte_length * char_count in
            Bytes.set b ind @@ Bytes.get bytes bin
          done
        done;
        Hashtbl.replace characters c (Bytes.to_string b);
        let char_width = Bytes.get_uint8 bytes (offset - 9 - char_count + i) in
        if char_width > char_byte_length * 8 then
            failwith "Over-wide char";
        Hashtbl.replace char_widths c char_width;
        index + char_byte_length
      )
      offset
      (ascii_first -- ascii_last)
    in
    Font.{
      char_byte_length;
      space_x; space_y; characters; char_widths;
      height=1+char_bot_row - char_top_row;
      textures=Hashtbl.create 50;
    }
  in
  let fonts = List.map (fun offset -> create_font bytes offset) font_offsets |> Array.of_list in
  fonts

let main filename =
  Printf.printf "--- Font dump: %s\n" filename;
  let fonts = of_file filename in
  Printf.printf "Num fonts: %d\n" (Array.length fonts)

let load win =
  let fonts = of_file "./data/FONTS.RR" in
  Array.iter (Font.create_textures win) fonts;
  fonts

module Render = struct

  module R = Renderer

  type render_char =
    {
      c: char;
      x: int;
      y: int;
      ega_color: int;
    }

  type t = 
    {
      font_idx: int;
      chars: render_char list;
    }

  (* module OptionT = List.Traverse(Option) *)

  let render fonts ~to_render ~win =
    List.fold_left (fun acc {font_idx; chars} ->
      let font = fonts.(font_idx) in
      List.fold_left (fun _acc {c; x; y; ega_color} ->
        let char_tex = Hashtbl.find font.Font.textures c in
        R.render ~x ~y ~color:(Ega.get_rgb ega_color) win char_tex
      )
      acc
      chars
    )
    (Result.return ())
    to_render 

end

  (* Create a list of locations of chars to render *)
let write_str ?(color=15) idx str ~fonts ~x ~y : Render.t =
  let font = fonts.(idx) in
  let _, _, acc =
    String.fold (fun (x, y, acc) c ->
      if Char.equal c '\n' then
        (x, y + font.Font.height + font.space_y, acc)
      else
        let add = {Render.x; y; c; ega_color=color} in
        let acc = add::acc in
        let w = Font.get_letter_width font c in
        (x + w + font.space_x, y, acc))
    (x, y, [])
    str
  in
  {font_idx=idx; chars=acc}
