open Containers
open Re

module Pattern = struct

  type t =
    | Victim
    | SndOrg
    | RcvOrg
    | SndLoc
    | RcvLoc
    | HlpOrg
    | Object
      [@@deriving show, eq]

  let show v = show v
  |> String.uppercase_ascii
  |> (^) "$"

  let of_string = function
    | "$VICTIM" -> Some Victim
    | "$SNDORG" -> Some SndOrg
    | "$RCVORG" -> Some RcvOrg
    | "$SNDLOC" -> Some SndLoc
    | "$RCVLOC" -> Some RcvLoc
    | "$HLPORG" -> Some HlpOrg
    | "$OBJECT" -> Some Object
    | _ -> None
end

let get_lines ~pat =
  let pat_r = compile (seq [
    (* Pattern we're looking for *)
    seq [bol; str pat; eol];
    (* Could be other patterns too *)
    rep (seq [bol; char '*'; rep any; eol]);
    group (rep (compl [set "*"]))
  ]) in
  fun text ->
  try
    let subs = exec pat_r text in
    Group.get subs 1
    |> String.map (function '\n' -> ' ' | x -> x)
    |> String.drop 1 (* Drop first \n -> space *)
    |> Option.some
  with Not_found ->
    None

    (* Substitute a Pattern list into a string *)
let subst_pat patterns s =
  (* Separate commas and periods *)
  let s = String.flat_map (function
    | ',' -> " , "
    | '.' -> " . "
    | x -> Char.to_string x
  ) s
  in
  let words = String.split_on_char ' ' s in
  let words = List.map (fun word ->
    match Pattern.of_string word with
    | Some p -> List.assoc ~eq:Pattern.equal p patterns
    | None -> word
  ) words
  in
  let words = List.fold_left (fun acc word -> match acc, word with
    | x::xs, "," -> (x^",")::xs
    | x::xs, "." -> (x^".")::xs
    | _, "" -> acc
    | _, x -> x::acc)
    []
    words
    |> List.rev
  in
  String.concat " " words


