
(* Break cycle around agent.id *)
module Id = Engine.Int_id.Make()

include Id
