open Tetris.Types
open Tetris.Constants
open Tetris.Game

let () =
    let cfg = { width = 10; height = 24; piece_rots; offsets;
                scale = 20; margin_side = 5; margin_top = 2; colors } in
    let history = tetris cfg in
    Printf.printf "%d\n" (List.length (fst history))
