(** Lempel-Ziv-Welch compression algorithm *)
open Containers

(* TODO: dynamic bit sizes *)

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

module ReadBytes = struct
  type t = {
    buffer: Bytes.t;
    mutable byte_idx: int;
    mutable bit_idx: int;
  }

  let of_bytes b = {buffer=b; byte_idx=0; bit_idx=0}

  let eof b = b.byte_idx >= Bytes.length b.buffer

  let get_bits v num_bits =
    assert (num_bits >= 8);
    assert (num_bits <= 16);

    (*Printf.printf "byte_idx: %x, bit_idx: %d, d=0x%x\n" v.byte_idx v.bit_idx (Bytes.get_uint8 v.buffer v.byte_idx); *)

    (* get 3 bytes *)
    let word = Bytes.get_uint8 v.buffer v.byte_idx in
    let word =
      if v.byte_idx < Bytes.length v.buffer - 1 then
        let byte2 = Bytes.get_uint8 v.buffer (v.byte_idx + 1) in
        word lor (byte2 lsl 8)
      else word
    in
    let word =
      if v.byte_idx < Bytes.length v.buffer - 2 then
        let byte3 = Bytes.get_uint8 v.buffer (v.byte_idx + 2) in
        word lor (byte3 lsl 16)
      else word
    in
    (* Printf.printf "word=0x%x\n" word; *)

    (* shift right to get needed bits *)
    let word = word lsr v.bit_idx in
    (* Printf.printf "post-shift word=0x%x\n" word; *)

    (* mask out unneeded bits *)
    let mask = lnot (0x7FFFFFFF lsl num_bits) in
    let res = word land mask in

    let () =
      match num_bits with
      | 8  -> v.byte_idx <- v.byte_idx + 1
      | 16 -> v.byte_idx <- v.byte_idx + 2
      | _ ->
          v.byte_idx <- v.byte_idx + 1;
          v.bit_idx <- v.bit_idx + (num_bits - 8);
          if v.bit_idx >= 8 then begin
            v.byte_idx <- v.byte_idx + 1;
            v.bit_idx <- v.bit_idx - 8
          end
    in
    res

  let iter_bits v i f =
    let bit_size = ref i in
    while not @@ eof v do
      let b = f (get_bits v !bit_size) in
      bit_size := b
    done

  let fold_bits v i f ~zero =
    let bit_size = ref i in
    let acc_ref = ref zero in
    while not @@ eof v do
      let acc, b = f !acc_ref !bit_size (get_bits v !bit_size) in
      acc_ref := acc;
      bit_size := b;
    done;
    !acc_ref
end


(** decompress a list of output symbols to a string *)
let decompress compressed ~max_bit_size =
  (* build the dictionary *)
  let compressed = ReadBytes.of_bytes compressed in
  let dictionary = Hashtbl.create 397 in

  let reset () =
    let dict_size = 256 in
    Hashtbl.reset dictionary;
    for i=0 to dict_size - 1 do
      let str = String.make 1 (char_of_int i) in
      Hashtbl.add dictionary i str
    done
  in
  reset ();

  let result = Buffer.create 1024 in

  let w =
    let x = ReadBytes.get_bits compressed 9 in
    Hashtbl.find dictionary x
  in
  Buffer.add_string result w;

  let _ =
    (* Start at 257 rather than 256 for no reason *)
    ReadBytes.fold_bits compressed 9 ~zero:(w,257) @@
      fun (w,count) bit_size k ->
        let entry =
          match Hashtbl.find_opt dictionary k with
          | Some s ->
              (*Printf.printf "%d: Found %d(0x%x) bitsize=%d in dictionary: len %d\n" count k k bit_size (String.length s); *)
              s
          | None when k = count -> (* Only option *)
              (* Add first letter of last matched word *)
              w ^ String.sub w 0 1
          | _ ->
              raise @@
              ValueError(Printf.sprintf "Bad compressed k: %d(0x%x), bitsize=%d size=%d count=%d"
                k k bit_size (Hashtbl.length dictionary) count)
        in
        Buffer.add_string result entry;

        (* add (w ^ entry.[0]) to the dictionary *)
        Hashtbl.replace dictionary count (w ^ (String.sub entry 0 1));

        let bit_size =
          if count + 2 > 1 lsl bit_size then bit_size + 1 else bit_size
        in
        if bit_size > 11 (* max_bit_size + 3 *) then begin
          reset ();
          ((entry, 256), 9)
        end else
          ((entry, count+1), bit_size)
  in
  Buffer.to_bytes result

