include Engine.Utils

let dist region loc1 loc2 =
  Region.dist_mult region * classic_dist loc1 loc2

module Map = struct
  module Make(O:OrderedType) = struct
    include Map.Make(O)

    let incr_cash k x v = incr_f ~combine:Money.add ~zero:Money.zero k x v

    let merge_add_cash v1 v2 = merge_f ~combine:Money.add ~zero:Money.zero v1 v2

    let sum_cash f v = sum_f ~combine:Money.add ~zero:Money.zero f v

    let total_cash v = sum_cash (fun _ x -> x) v
  end
end

let clip_cash v ~min ~max = clip (Money.to_int v) ~min ~max |> Money.of_int

module List = struct
  include List
  let sum_cash f v = List.fold_left (fun acc x -> Money.(acc + f x)) Money.zero v
end

