open Containers
open Iter.Infix

type font =
  {
    ascii_first: char;
    ascii_last: char;
    char_byte_length: int;
    char_top_row: int;
    char_bot_row: int;
    space_x: int;
    space_y: int;
    char_widths: (char, int) Hashtbl.t;
    characters: (char, string) Hashtbl.t;
  }

let main filename =
  Printf.printf "--- Font dump: %s" filename;

  let filepath = Filename.remove_extension filename in
  let destpath = filepath ^ ".png" in

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
  let create_font offset =
    let ascii_first = Bytes.get (offset - 8) in
    let ascii_last = Bytes.get (offset - 7) in
    let char_byte_length = Bytes.get (offset - 6) in
    let char_top_row = Bytes.get (offset - 5) in
    let char_bot_row = Bytes.get (offset - 4) in
    let space_x = Bytes.get (offset - 3) in
    let space_y = Bytes.get (offset - 2) in
    let space_y = Bytes.get (offset - 2) in
    Iter.iterate (
      space_x
    )
  in
  ()



let () =
  main Sys.argv.(1)



