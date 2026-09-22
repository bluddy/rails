open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
open! Containers

(* Hq stuff requiring case data structures *)

module G = Case.G

let kind (c:Case.t) org_id loc_id =
  Hq.get_kind org_id loc_id c.d.locs (G.orgs c) (G.roles c) (G.agents c) c.s.mm c.world

let known_to_org org1_id org2_id loc_id (c:Case.t) =
  Hq.known_to_org org1_id org2_id loc_id (G.locs c) (G.orgs c) (G.roles c) (G.agents c) c.s.mm c.world
