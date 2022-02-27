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

module BitReader = struct
  type t = {
    buffer: int;
    mutable bit_idx: int;
    source: (int * char) Seq.t;
  }

  let of_seq source = {buffer=0; bit_idx=0; source}

  let get_bits v num_bits =
    assert (num_bits >= 8);
    assert (num_bits <= 16);

    (* Printf.printf "byte_idx: %x, bit_idx: %d, d=0x%x\n" (v.byte_idx + v.report_offset) v.bit_idx (Bytes.get_uint8 v.buffer v.byte_idx); *)

    let bytes_to_read =
      if num_bits > 8 then 2 else 1
    in
    let byte_offset =
      match bit_idx with
      | 0 -> 0
      | _ -> 1
    in
    let get_byte () = Char.code @@ snd @@ Seq.head_exn v.source in

    (* Add one byte *)
    v.buffer <- v.buffer lor ((get_byte ()) lsl 8 * byte_offset);

    (* Add second byte if needed *)
    if bytes_to_read > 1 then
      v.buffer <- v.buffer lor ((get_byte ()) lsl 8 * (byte_offset + 1));

    (* shift right to get needed bits *)
    let word = v.buffer lsr v.bit_idx in
    (* Printf.printf "post-shift word=0x%x\n" word; *)

    (* mask out unneeded bits *)
    let mask = lnot (0x7FFFFFFF lsl num_bits) in
    let res = word land mask in

    (* Update buffer for next time *)
    let () =
      match num_bits with
      | 8  -> v.buffer <- v.buffer lsr 8
      | 16 -> v.buffer <- v.buffer lsr 16
      | _ ->
          v.buffer <- v.buffer lsr 8
          v.bit_idx <- v.bit_idx + (num_bits - 8);
          (* Reduce bit_idx >= 8 to < 8 *)
          if v.bit_idx >= 8 then begin
            v.buffer <- v.buffer lsr 8
            v.bit_idx <- v.bit_idx - 8
          end
    in
    res

end


(** decompress a list of output symbols to a string *)
let decompress ?(report_offset=0) ?(suppress_error=false) compressed ~max_bit_size =
  (* build the dictionary *)
  let compressed = ReadBytes.of_bytes compressed ~report_offset in
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
    try begin
      (* Start at 257 rather than 256 for no reason *)
      ReadBytes.fold_bits compressed 9 ~zero:(w,257) @@
        fun (w,count) bit_size k ->
          let entry =
            match Hashtbl.find_opt dictionary k with
            | Some s ->
                (* Printf.printf "%d: Found %d(0x%x) bitsize=%d in dictionary: len %d\n" count k k bit_size (String.length s); *)
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
          if bit_size > max_bit_size then begin
            reset ();
            ((entry, 256), 9)
          end else
            ((entry, count+1), bit_size)
    end with ValueError _ as e ->
      if suppress_error then ("", 0) else raise e
  in
  Buffer.to_bytes result

