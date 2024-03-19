open! Containers
open! Ppx_yojson_conv_lib.Yojson_conv.Primitives
module C = Constants
module R = Renderer

(* let compute_ytd backend player last_year = *)
(*   (* Compute the current balance sheet *) *)
(*   let operating_funds = player.cash in *)
(*   let treasury_stock = player.treasury_stock in *)
(*   let other_rr_stock = player.other_rr_stock in *)
(*   () *)


let render win (s:State.t) =
  Ui_common.render_full_screen_frame win s.textures s.ui.dims;
  Fonts.Render.write_shadow win s.fonts ~idx:2 ~color:Ega.gray ~x:32 ~y:8 @@
    Printf.sprintf "Balance Sheet: %d" s.backend.year;
  ()

    
