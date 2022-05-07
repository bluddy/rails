open Containers

let clip v ~min ~max =
  if v >= min then 
    if v <= max then
      v
    else
      max
  else
    min

let random_int maxval r =
  if maxval = 0 then 0 else Random.int maxval r
  

module List = struct
  let modify_at_idx i f l0 =
    let rec loop i acc = function
      | [] -> l0
      | y::ys when i=0 ->
          List.rev_append acc ((f y) :: ys)
      | y::ys ->
          loop (i-1) (y::acc) ys
    in
    loop i [] l0

  let modify_make_at_idx i f l0 =
    let rec loop i acc = function
      | [] -> l0, None
      | y::ys when i=0 ->
          let y', prod = f y in
          List.rev_append acc (y' :: ys), Some prod
      | y::ys ->
          loop (i-1) (y::acc) ys
    in
    loop i [] l0
end

let scan ~range ~x ~y ~max_x ~max_y ~f =
  let min_x = max 0 (x-range) in
  let max_x = min max_x (x+range) in
  let min_y = max 0 (y-range) in
  let max_y = min max_y (y+range) in
  let exception Found in
  try
    for i=min_y to max_y do
      for j=min_x to max_x do
        if f j i then
          raise_notrace Found
      done
    done;
    false
  with
  | Found -> true


