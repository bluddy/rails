open Containers

let of_stringi ?(start=0) ?len s =
  let len = match len with
    | None -> String.length s - start
    | Some n -> assert (n + start < String.length s); n in
  let i = ref start in
  fun () ->
    if !i >= start + len
    then None
    else begin
      let x = s.[!i] in 
      let j = !i in
      incr i;
      Some (j, x)
    end

let last_i = ref 0

let get_bytei s : int =
  let i, c = Option.get_exn @@ Gen.get s in
  last_i := i;
  Char.code c

let junki s =
  let i, _ = Option.get_exn @@ Gen.get s in
  last_i := i;
  ()

let get_wordi s : int =
  let word = get_bytei s in
  word lor ((get_bytei s) lsl 8)

let pos () = !last_i

let get_byte s : int =
  Char.code @@ Option.get_exn @@ Gen.get s

let get_word s : int =
  let word = get_byte s in
  word lor ((get_byte s) lsl 8)

let take n gen =
  assert (n >= 0);
  let count = ref 0 in  (* how many yielded elements *)
  fun () ->
    if !count = n || !count = ~-1
    then None
    else match gen() with
      | None -> count := ~-1; None   (* indicate stop *)
      | (Some _) as x ->
          incr count;
          Printf.printf "take: %d\n" !count;
          x

