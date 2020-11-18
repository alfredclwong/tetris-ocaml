open Tetris.Types
open Tetris.Constants
open Tetris.Game

let () =
    let cfg = { width = 10; height = 24; piece_rots; offsets } in
    let history = tetris cfg in
    Printf.printf "%d\n" (List.length (fst history))
