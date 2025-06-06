open Containers
open Iter.Infix

let src = Logs.Src.create "font" ~doc:"Font"
module Log = (val Logs.src_log src: Logs.LOG)

let debug = false

module Font = struct

(* fonts:
  0: fancy olde style
  1: all-caps
  2: large
  3: tiny
  4: standard
*)

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
  let width = get_width font c in
  (* Printf.printf "char[%c] width[%d]\n" c width; *)
  let pixels = Ndarray.empty Int8_unsigned [|font.height; width; 4|] in
  ignore @@ write_letter font ~pixels c;
  pixels

let get_letter_width font c =
  try
    Hashtbl.find font.char_widths c
  with Not_found ->
    Log.debug (fun f -> f "Character \"%c\" not found in font" c);
    7

let get_str_w_h ?(skip_amp=false) font str =
  let _, w, h =
    String.fold (fun ((x, max_x, y) as acc) c ->
      match c with
      | '&' when skip_amp -> acc
      | '\n' -> 0, max_x, y + font.space_y + font.height
      | _ ->
        let w = get_letter_width font c in
        let x = x + w + font.space_x in
        x, max max_x x, y
    )
    (0, 0, font.height + font.space_y)
    str
  in 
  w, h

  module R = Renderer

  let create_textures win v =
    Hashtbl.keys v.characters (fun c ->
      let width = get_width v c in
      if width > 0 then (
        let ndarray = get_letter v c in
        let tex = R.Texture.make win ndarray in
        Hashtbl.replace v.textures c tex
      )
    )

  let write_char win font ?(color=Ega.white) c ~x ~y =
    try
      let char_tex = Hashtbl.find font.textures c in
      R.Texture.render ~x ~y ~color win char_tex;
      let w = get_letter_width font c in
      (x + w + font.space_x, y)
    with
    | Not_found ->
        Log.err (fun f -> f "char '%c' not found in font." c); 
        x, y

    (* Write a string.
       active_color: color for automatically highlighting chars with &
    *)
  let write ?active_color win font ~color str ~x ~y =
    let x_first = x in (* keep starting column *)
    String.fold (fun (active, x, y) c ->
      match c, active_color with
      | '&', Some _ ->
          (true, x, y)
      | '\n', _ ->
          (false, x_first, y + font.height + font.space_y)
      | _, Some active_color when active ->
          let x, y = write_char win font ~color:active_color c ~x ~y in
          false, x, y
      | _ ->
          let x, y = write_char win font ~color c ~x ~y in
          false, x, y
    )
    (false, x, y)
    str
    |> ignore

end

type t = Font.t array

let get_font v idx = v.(idx)

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
  let create_font i bytes offset =
    let ascii_first = Bytes.get_uint8 bytes @@ offset - 8 in
    let ascii_last = Bytes.get_uint8 bytes @@ offset - 7 in
    let char_byte_length = Bytes.get_uint8 bytes @@ offset - 6 in
    let min_width = Bytes.get_uint8 bytes @@ offset - 5 in
    let min_height = Bytes.get_uint8 bytes @@ offset - 4 in
    let space_x = Bytes.get_uint8 bytes @@ offset - 3 in
    let space_y = Bytes.get_uint8 bytes @@ offset - 2 in
    let char_count = ascii_last - ascii_first + 1 in
    let characters = Hashtbl.create 50 in
    let char_widths = Hashtbl.create 50 in
    if debug then (
      Printf.printf "font[%d] offset[0x%x]\n" i offset;
      Printf.printf "ascii_first[%d]\n" ascii_first;
      Printf.printf "ascii_last[%d]\n" ascii_last;
      Printf.printf "char_byte_length[%d]\n" char_byte_length;
      Printf.printf "min_width[%d]\n" min_width;
      Printf.printf "min_height[%d]\n" min_height;
      Printf.printf "space_x[%d]\n" space_x;
      Printf.printf "space_y[%d]\n" space_y;
    );

    let _ =
      Iter.foldi (fun index i c ->
        let c = char_of_int c in
        let i = i+1 in
        (* NOTES: small font: char_bot_row=3, char_top_row=4 (0,X) for most
           Could char_bot_row be min_width, char_top_row is min_height?
        *)
        let b = Bytes.create @@ min_height * char_byte_length in
        for row = 0 to min_height - 1 do
          for col = 0 to char_byte_length - 1 do
            let ind = row * char_byte_length + col in
            let bin = index + col + row * char_byte_length * char_count in
            Bytes.set b ind @@ Bytes.get bytes bin
          done
        done;
        Hashtbl.replace characters c (Bytes.to_string b);
        let char_width = Bytes.get_uint8 bytes (offset - 9 - char_count + i) in
        let char_width = char_width + min_width in
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
      space_x; space_y;
      characters;
      char_widths;
      height=min_height;
      textures=Hashtbl.create 50;
    }
  in
  let fonts = List.mapi (fun i offset -> create_font i bytes offset) font_offsets |> Array.of_list in
  fonts

let main filename =
  Printf.printf "--- Font dump: %s\n" filename;
  let fonts = of_file filename in
  Printf.printf "Num fonts: %d\n" (Array.length fonts);
  Array.iteri (fun i font ->
    Printf.printf "Font %d:\n" i;
    print_endline @@ Font.show font;
  )
  fonts


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

  (* Render the list of t that memoizes font rendering *)
  let render ?(x=0) ?(y=0) ?color fonts ~to_render ~win =
    let x_off, y_off = x, y in
    List.fold_left (fun acc {font_idx; chars} ->
      let font = fonts.(font_idx) in
      List.fold_left (fun _acc {c; x; y; ega_color} ->
        let char_tex = Hashtbl.find font.Font.textures c in
        let x, y = x + x_off, y + y_off in
        let color = match color with
          | None -> Ega.get_rgba ega_color
          | Some c -> c
        in
        R.Texture.render ~x ~y ~color win char_tex
      )
      acc
      chars
    )
    ()
    to_render 

let write_char win fonts ~color ~idx c ~x ~y =
  Font.write_char win ~color fonts.(idx) c ~x ~y

let write win fonts ?active_color ~color ~idx str ~x ~y =
  Font.write ?active_color win fonts.(idx) ~color str ~x ~y

let write_shadow win fonts ~color ~idx str ~x ~y =
  write win fonts ~color:Ega.black ~idx str ~x:(x+1) ~y:(y+1);
  write win fonts ~color ~idx str ~x ~y

end

  (* Create a list of locations of chars to render *)
let write_str ?(color=15) idx str ~fonts ~x ~y : Render.t =
  let font = fonts.(idx) in
  let _, _, acc =
    let x_first = x in
    String.fold (fun (x, y, acc) c ->
      if Char.equal c '\n' then
        (x_first, y + font.Font.height + font.space_y, acc)
      else
        let add = {Render.x; y; c; ega_color=color} in
        let acc = add::acc in
        let w = Font.get_letter_width font c in
        (x + w + font.space_x, y, acc))
    (x, y, [])
    str
  in
  {font_idx=idx; chars=acc}

let get_str_w_h ?skip_amp ~fonts ~idx str =
  Font.get_str_w_h ?skip_amp fonts.(idx) str

