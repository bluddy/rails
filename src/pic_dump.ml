open Containers

let main filename =
  let bytes =
    IO.with_in filename @@
      fun in_channel -> IO.read_all_bytes in_channel
  in
  let compressed = ref [] in
  for i=0 to Bytes.length bytes - 1 do
    let v = Bytes.get_uint8 bytes i in
    compressed := v :: !compressed;
  done;

  compressed := List.rev !compressed;

  let decompressed = Lzw.decompress !compressed in

  let buf = Buffer.create 1024 in
  List.iter (fun s -> Buffer.add_string buf s) decompressed;

  print_endline "--- Pic dump";
  Printf.printf "Length compressed: %d\n" (List.length !compressed);
  Printf.printf "Length decompressed: %d\n" (Buffer.length buf);
  ()

let () =
  main Sys.argv.(1)



