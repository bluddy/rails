(** Lempel-Ziv-Welch compression algorithm *)
open Containers

(* TODO: dynamic bit sizes *)

let debug = ref false

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
    mutable buffer: int;
    mutable bit_idx: int;
    mutable length: int; (* in bytes *)
    source: (int * char) Gen.t;
  }

  let of_stream source = {buffer=0; bit_idx=0; length=0; source}

  let get_bits v num_bits =
    assert (num_bits >= 8);
    assert (num_bits <= 16);

    let bytes_to_read =
      let have_bits = v.length * 8 - v.bit_idx in
      let need_bits = num_bits - have_bits in
      int_of_float @@ ceil @@ float_of_int need_bits /. 8.
    in

    if !debug then
      Printf.printf "0x%x: get %d bits, before buffer:0x%x, bitidx:%d, length:%d, btr:%d\n" (My_gen.pos ()) num_bits v.buffer v.bit_idx v.length bytes_to_read; (* debug *)

    (* Add bytes *)
    for _i=0 to bytes_to_read - 1 do
      v.buffer <- v.buffer lor ((My_gen.get_bytei v.source) lsl (8 * v.length));
      v.length <- v.length + 1;
    done;

    (* shift right to get needed bits *)
    let word = v.buffer lsr v.bit_idx in
    (* Printf.printf "post-shift word=0x%x\n" word; *)

    (* mask out unneeded bits *)
    let mask = lnot (0x7FFFFFFF lsl num_bits) in
    let res = word land mask in

    if !debug then
      Printf.printf "0x%x: get %d bits, after buffer:0x%x bitidx:%d length:%d, read 0x%x\n" (My_gen.pos ()) num_bits v.buffer v.bit_idx v.length res; (* debug *)

    (* Update buffer for next time *)
    v.buffer  <- v.buffer lsr 8;
    v.length  <- v.length - 1;
    v.bit_idx <- v.bit_idx + num_bits - 8;
    (* Reduce bit_idx >= 8 to < 8 *)
    if v.bit_idx >= 8 then begin
      v.buffer  <- v.buffer lsr 8;
      v.length  <- v.length - 1;
      v.bit_idx <- v.bit_idx - 8;
    end;
    res
end


(** decompress a stream of output symbols to a string *)
let decompress (compressed:(int*char) Gen.t) ~max_bit_size : char Gen.t =
  (* build the dictionary *)
  let compressed = BitReader.of_stream compressed in
  let dictionary = Hashtbl.create 397 in

  let reset () =
    (* Reset the dictionary *)
    let dict_size = 256 in
    Hashtbl.reset dictionary;
    for i=0 to dict_size - 1 do
      let str = String.make 1 (char_of_int i) in
      Hashtbl.add dictionary i str
    done
  in
  reset ();

  let w =
    let x = BitReader.get_bits compressed 9 in
    Hashtbl.find dictionary x
  in

  let add_string s = Gen.of_string s in

  let stream1 : char Gen.t = add_string w in


  let stream2 : char Gen.t =
      Gen.flatten @@
      (* Start at 257 rather than 256 for no reason *)
      Gen.unfold (fun (w, count, bit_size) ->
          let k = BitReader.get_bits compressed bit_size in

          let entry =
            match Hashtbl.find_opt dictionary k with
            | Some s ->
                if !debug then
                  Printf.printf "%d: Found %d(0x%x) bitsize=%d in dictionary: len %d\n"
                   count k k bit_size (String.length s); (* debug *)
                s
            | None when k = count -> (* Only option *)
                (* Add first letter of last matched word *)
                w ^ String.sub w 0 1
            | _ ->
                raise @@
                ValueError(Printf.sprintf "Bad compressed k: %d(0x%x), offset=0x%x bitsize=%d size=%d count=%d"
                  k k (My_gen.pos ()) bit_size (Hashtbl.length dictionary) count)
          in
          (* add (w ^ entry.[0]) to the dictionary *)
          Hashtbl.replace dictionary count (w ^ (String.sub entry 0 1));

          let result: char Gen.t = add_string entry in

          let bit_size =
            if count + 2 > 1 lsl bit_size
            then bit_size + 1 else bit_size
          in

          if bit_size > max_bit_size then begin
            reset ();
            Some (result, (entry, 256, 9))
          end else
            Some (result, (entry, count+1, bit_size))
      )
      (w, 257, 9)
  in
  Gen.append stream1 stream2

(*
The image data is stored as LZW compressed RLE stream. The LZW resets when the dictionary gets full (i.e, there's no separate reset signal).
Under the LZW the data is compressed with RLE, so that if a pixel byte is 0x90, the previous pixel is repeated as many times as the next byte says; if the repeat value is 0, the pixel value is 0x90.

To reiterate, the RLE works this way:
aa 90 bb
if bb = 0, output is "aa 90"
if bb != 0, output is "aa" * (bb+1)

And yes, if you want a stream of 90's, you do 90 00 90 xx.

*)

let decode_rle (stream: char Gen.t) : char Gen.t =

  let get_byte s : int = Char.code @@ Gen.get_exn s in
  let add_byte b = Gen.return @@ Char.chr b in

  let result : char Gen.t =
    Gen.flatten @@
    Gen.unfold (fun (rle, last_val) ->
      match get_byte stream with
      | 0x90 -> Some (Gen.empty, (true, last_val))
      | 0 when rle -> Some (add_byte 0x90, (false, 0x90))
      | x when rle ->
          (* repeat x-1 times *)
          let g = Gen.init ~limit:(x-1) (fun _ -> Char.chr last_val) in
          Some (g, (false, last_val))
      | x -> Some (add_byte x, (rle, x))
    )
    (false, 0)
  in
  result

