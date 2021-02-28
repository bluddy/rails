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
    height: int;
    char_count: int;
  }

let main filename =
  Printf.printf "--- Font dump: %s\n" filename;

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
    let ascii_first, ascii_last = char_of_int ascii_first, char_of_int ascii_last in
    {
      ascii_first; ascii_last;
      char_byte_length; char_top_row; char_bot_row;
      space_x; space_y; char_count; characters; char_widths;
      height=1+char_bot_row - char_top_row;
    }
  in
  let fonts = List.map (fun offset -> create_font bytes offset) font_offsets in
  Printf.printf "Num fonts: %d\n" (List.length fonts)




