open Printf
open Graphics

(* util *)
let rec zip two_lists =
    match two_lists with
    | [], [] -> []
    | h1::t1, h2::t2 -> (h1, h2)::(zip (t1, t2))
    | _, _ -> failwith "tried to zip two lists with different lengths"
let (--) a b =
    let rec aux i acc =
        if i<a then acc else aux (i-1) (i::acc)
    in aux (b-1) []
let (---) a b =
    Array.init (b-a) (fun i -> i+a)
let tuple_diff (a,b) (c,d) =
    (a-c,b-d)
(* util *)

let rotate (blocks, i, (x,y), r0) dir board =
    let offsets =
        let jzlts =
            [|[|( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0)|];
              [|( 0, 0); ( 1, 0); ( 1,-1); ( 0, 2); ( 1, 2)|];
              [|( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0)|];
              [|( 0, 0); (-1, 0); (-1,-1); ( 0, 2); (-1, 2)|]|] and
        i = [|[|( 0, 0); (-1, 0); ( 2, 0); (-1, 0); ( 2, 0)|];
              [|(-1, 0); ( 0, 0); ( 0, 0); ( 0, 1); ( 0,-2)|];
              [|(-1, 1); ( 1, 1); (-2, 1); ( 1, 0); (-2, 0)|];
              [|( 0, 1); ( 0, 1); ( 0, 1); ( 0,-1); ( 0, 2)|]|] and
        o = [|[|( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0)|];
              [|( 0,-1); ( 0,-1); ( 0,-1); ( 0,-1); ( 0,-1)|];
              [|(-1,-1); (-1,-1); (-1,-1); (-1,-1); (-1,-1)|];
              [|(-1, 0); (-1, 0); (-1, 0); (-1, 0); (-1, 0)|]|] in
        [|jzlts; i; jzlts; jzlts; o; jzlts; jzlts|] in
    let n_rotations = Array.length offsets.(0) and n_kicks = Array.length offsets.(0).(0) in
    let r1 = (r0+dir) mod n_rotations in
    let (dx,dy) = tuple_diff offsets.(i).(r0).(0) offsets.(i).(r1).(0) in
    let rotate_point (px,py) = if dir==(-1) then (-py,px) else (py,-px) in
    ((Array.map rotate_point blocks), i, (x+dx,y+dy), r1)

let draw (blocks, i, (x, y), _) s =
    let colors = [|red; green; blue; yellow; cyan; magenta; black|] in
    let color = colors.(i) in
    let draw_block (px, py) = fill_rect (s*(x+px)) (s*(y+py)) (s-1) (s-1) in
    set_color color;
    Array.iter draw_block blocks

let () =
    let width = 10 and height = 24 and scale = 20 and
    pieces =
        [|[|(-1, 0); (-1, 1); ( 0, 0); ( 1, 0)|];
          [|(-1, 0); ( 0, 0); ( 1, 0); ( 2, 0)|];
          [|(-1, 1); ( 0, 0); ( 0, 1); ( 1, 0)|];
          [|(-1, 0); ( 0, 0); ( 1, 0); ( 1, 1)|];
          [|( 0, 0); ( 0, 1); ( 1, 0); ( 1, 1)|];
          [|(-1, 0); ( 0, 0); ( 0, 1); ( 1, 0)|];
          [|(-1, 0); ( 0, 0); ( 0, 1); ( 1, 1)|]|] in
    let n_pieces = Array.length pieces in
    let spawns = Array.init n_pieces (fun i -> (4,i*4+1)) in

    open_graph (sprintf (" %dx%d") (scale*width) (scale*height));
    let _pieces = ref (Array.map (fun i -> (pieces.(i), i, spawns.(i), 0)) (0---n_pieces)) in
    let c = ref ' ' in
    while !c!='q' do
        clear_graph ();
        Array.iter (fun piece -> draw piece scale) !_pieces;
        c := read_key ();
        _pieces := Array.map (fun piece -> rotate piece 1 0) !_pieces
    done
