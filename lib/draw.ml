open Graphics
open Types

let draw { score; board = b; pip; hold; queue = q; _ } { width = w; height = h; scale = s; margin_side = ms; colors; piece_rots; _ } =
  let draw_text ?(c=black) x y t = moveto (s*x) (s*y); set_color c; draw_string t in
  let draw_line x0 y0 x1 y1 c = moveto (s*x0) (s*y0); set_color c; lineto (s*x1) (s*y1) in
  let draw_block x y c = set_color c; fill_rect (s*x) (s*y) (s-1) (s-1) in
  let draw_blocks x y c blocks = List.iter (fun (bx, by) -> draw_block (x+bx) (y+by) c) blocks in
  let draw_score x y =
      List.iter2 (fun (x, y) t -> draw_text x y t)
      [ (x, y); (x, y-1) ] [ "SCORE"; (Printf.sprintf "%d" score) ] in
  let draw_board _ =
      Array.iteri (fun x col ->
          Array.iteri (fun y i ->
              if i > -1 then draw_block (ms+x) y colors.(i))
          col)
      b in
  let draw_pip = function
      | None -> ()
      | Some {i; loc = (x, y); r; ghost} ->
          let dgray = rgb 191 191 191 in
          let blocks = List.nth (List.nth piece_rots i) r in
          draw_blocks (ms+x) ghost dgray blocks;
          draw_blocks (ms+x) y colors.(i) blocks in
  let rec draw_queue x y depth = function
      | [] -> draw_text x y "QUEUE"
      | i::tl ->
          let blocks = (List.hd (List.nth piece_rots i)) in
          draw_blocks (x+1) (y-(depth+1)*3) colors.(i) blocks;
          draw_queue x y (depth+1) tl in
  let draw_hold x y depth = function
      | None -> draw_text x y "HOLD"
      | Some hold -> draw_queue x y depth [hold] in
  let draw_grid _ =
      let rows = List.init (h-1) (fun i -> (0, (i+1), w, (i+1))) in
      let cols = List.init (w-1) (fun i -> ((i+1), 0, (i+1), h)) in
      let frame = [ (0, 0, 0, h); (0, h, w, h); (w, h, w, 0) ] in
      let gray = rgb 223 223 223 in
      List.iter (fun (c, lines) ->
          List.iter (fun (x0, y0, x1, y1) ->
              draw_line (ms+x0) y0 (ms+x1) y1 c)
          lines)
      [(gray, rows); (gray, cols); (black, frame)] in
  clear_graph ();
  draw_score (ms+w+1) (h-15);
  draw_queue (ms+w+1) (h-1) 0 q;
  draw_hold 1 (h-1) 0 hold;
  draw_board (); draw_pip pip; draw_grid ()
