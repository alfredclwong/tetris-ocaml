open Draw
open Types
open Logic

let tetris = function { width = w; height = h; scale = s;
                        margin_side = ms; margin_top = mt; _ } as cfg ->
    Random.self_init ();
    Graphics.open_graph (Printf.sprintf " %dx%d" ((w+ms*2)*s) ((h+mt)*s));
    let rec loop = function
        | ((curr_state::_ as state_history), move_history) as history -> (
            draw curr_state cfg;
            let move =
                match Graphics.read_key () with
                | 'q' -> Some (Quit)
                | 'j' -> Some (Rot CCW)
                | 'k' -> Some (Rot CW)
                | 'l' -> Some (Hold)
                | 'w' -> Some (Drop Hard)
                | 'a' -> Some (Shift L)
                | 's' -> Some (Drop Soft)
                | 'd' -> Some (Shift R)
                | _   -> None in
            match tick curr_state cfg move with
            | None -> history
            | Some next_state -> loop (next_state::state_history, move_history))
        | ([], _) ->
            let score = 0 and board = Array.make_matrix w h (-1) and
                pip = None and hold = None and queue = [] and bag = [] in
            let state0 = {score; board; pip; hold; queue; bag} in
            loop ([state0], []) in
    loop ([], [])
