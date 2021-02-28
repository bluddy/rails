
let chunk_writer_of_out_channel och =
  function
  | `String x ->
    ( try Ok (output_string och x) with
      | _ -> close_out och; Error `Write_error)
  | `Close ->
      close_out och; Ok ()

let chunk_writer_of_path fn =
  chunk_writer_of_out_channel (open_out_bin fn)

