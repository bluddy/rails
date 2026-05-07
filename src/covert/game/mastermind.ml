open! Containers

type t = {
  org: Org.Id.t;
  loc: Loc.Id.t;
  known: Known_data.Set.t;
}
