(** Lempel-Ziv-Welch compression algorithm *)
open Containers

(** compress a string to a list of output symbols *)
let compress uncompressed =
  (* build the dictionary *)
  let dict_size = 256 in
  let dictionary = Hashtbl.create 397 in
  for i=0 to 255 do
    let str = String.make 1 (char_of_int i) in
    Hashtbl.add dictionary str i
  done;

  let f  (w, dict_size, result) c =
    let c = String.make 1 c in
    let wc = w ^ c in
    if Hashtbl.mem dictionary wc then
      (wc, dict_size, result)
    else
      begin
        (* add wc to the dictionary *)
        Hashtbl.add dictionary wc dict_size;
        let this = Hashtbl.find dictionary w in
        (c, dict_size + 1, this::result)
      end
  in
  let w, _, result =
    String.fold f ("", dict_size, []) uncompressed
  in

  (* output the code for w *)
  let result =
    if String.length w = 0
    then result
    else (Hashtbl.find dictionary w) :: result
  in
  (List.rev result)

exception ValueError of string

(** decompress a list of output symbols to a string *)
let decompress compressed =
  (* build the dictionary *)
  let dict_size = 256 in
  let dictionary = Hashtbl.create 397 in
  for i=0 to dict_size - 1 do
    let str = String.make 1 (char_of_int i) in
    Hashtbl.add dictionary i str
  done;

  let w, compressed =
    match compressed with
    | hd::tl -> String.make 1 @@ char_of_int hd, tl
    | [] -> failwith "empty input"
  in

  let result = w::[] in

  let result, _, _ =
    List.fold_left (fun (result, w, dict_size) k ->
      let entry =
        if Hashtbl.mem dictionary k then
          Hashtbl.find dictionary k
        else if k = Hashtbl.length dictionary then
          w ^ (String.make 1 w.[0])
        else
          raise(ValueError(Printf.sprintf "Bad compressed k: %d" k))
      in
      let result = entry :: result in

      (* add (w ^ entry.[0]) to the dictionary *)
      Hashtbl.add dictionary dict_size (w ^ (String.make 1 entry.[0]));
      (result, entry, dict_size + 1)
    ) (result, w, dict_size)
    compressed
  in
  (List.rev result)
