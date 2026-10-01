open Containers

let compress_string ?(level = 6) str =
  let in_pos = ref 0 in
  let str_len = String.length str in
  let refill buf =
    let n = min (Bytes.length buf) (str_len - !in_pos) in
    Bytes.blit_string str !in_pos buf 0 n;
    in_pos := !in_pos + n;
    n
  in
  let b = Buffer.create (str_len / 2 + 32) in
  let flush buf len =
    Buffer.add_subbytes b buf 0 len
  in
  Zlib.compress ~level refill flush;
  Buffer.contents b

let decompress_string str =
  let in_pos = ref 0 in
  let str_len = String.length str in
  let refill buf =
    let n = min (Bytes.length buf) (str_len - !in_pos) in
    Bytes.blit_string str !in_pos buf 0 n;
    in_pos := !in_pos + n;
    n
  in
  let b = Buffer.create (str_len * 2 + 32) in
  let flush buf len =
    Buffer.add_subbytes b buf 0 len
  in
  try
    Zlib.uncompress refill flush;
    Ok (Buffer.contents b)
  with
  | Zlib.Error (arg, msg) -> Error (Printf.sprintf "Zlib error (%s): %s" arg msg)
  | exn -> Error (Printexc.to_string exn)

let is_likely_uncompressed s =
  let s_trimmed = String.trim s in
  String.starts_with ~prefix:"{" s_trimmed

let read_save_data raw_data =
  if String.length raw_data = 0 then
    Error "Save file is empty"
  else if is_likely_uncompressed raw_data then
    Ok raw_data
  else
    decompress_string raw_data

let read_save_file path =
  try
    if not (Sys.file_exists path) then
      Error (Printf.sprintf "File does not exist: %s" path)
    else
      let ic = open_in_bin path in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          let len = in_channel_length ic in
          let raw = really_input_string ic len in
          read_save_data raw)
  with exn ->
    Error (Printexc.to_string exn)

let write_save_file ?(level = 6) path data =
  let tmp_path = Printf.sprintf "%s.%d.tmp" path (Stdlib.Random.int 1000000) in
  try
    let compressed = compress_string ~level data in
    let oc = open_out_bin tmp_path in
    Fun.protect
      ~finally:(fun () -> close_out_noerr oc)
      (fun () ->
        output_string oc compressed;
        flush oc);
    (try Sys.rename tmp_path path
     with _ ->
       (try Sys.remove path with _ -> ());
       Sys.rename tmp_path path);
    Ok ()
  with exn ->
    (try Sys.remove tmp_path with _ -> ());
    Error (Printexc.to_string exn)
