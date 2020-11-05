open Printf
open Graphics

let pieces =
    [|[|(-1, 0); (-1, 1); ( 0, 0); ( 1, 0)|];
      [|(-1, 0); ( 0, 0); ( 1, 0); ( 2, 0)|];
      [|(-1, 1); ( 0, 0); ( 0, 1); ( 1, 0)|];
      [|(-1, 0); ( 0, 0); ( 1, 0); ( 1, 1)|];
      [|( 0, 0); ( 0, 1); ( 1, 0); ( 1, 1)|];
      [|(-1, 0); ( 0, 0); ( 0, 1); ( 1, 0)|];
      [|(-1, 0); ( 0, 0); ( 0, 1); ( 1, 1)|]|]

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
let shuffle d = (* TODO: http://okmij.org/ftp/Haskell/perfect-shuffle.txt *)
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond
(* util *)

let check_point (x,y) board =
    let width = Array.length !board and height = Array.length !board.(0) in
    0<=x && x<width && 0<=y && (y>=height || !board.(x).(y)==(-1))

let rotate piece dir board =
    let (blocks, i, (x,y), r0) = !piece in
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
    let r1 = (r0+dir+n_rotations) mod n_rotations in
    let rotate_point (px,py) = if dir==(-1) then (-py,px) else (py,-px) in
    let rec check remaining kick =
        if kick < n_kicks then
            let (dx,dy) = tuple_diff offsets.(i).(r0).(kick) offsets.(i).(r1).(kick) in
            match remaining with
            | [] -> piece := ((Array.map rotate_point blocks), i, (x+dx,y+dy), r1)
            | (px,py)::tl ->
                let (px,py) = rotate_point (px, py) in
                if check_point (x+px+dx,y+py+dy) board then check tl kick
                else check (Array.to_list blocks) (kick+1) in
    check (Array.to_list blocks) 0

let clear_lines board =
    let width = Array.length !board and height = Array.length !board.(0) in
    let clear_line y =
        for i = 0 to (width-1) do
            for j = y to (height-2) do !board.(i).(j) <- !board.(i).(j+1) done;
            !board.(i).(height-1) <- (-1);
        done in
    let rec check x y =
        if y < height then (
            if x == width then (clear_line y; check 0 y)
            else if !board.(x).(y) == (-1) then check 0 (y+1) else check (x+1) y) in
    check 0 0

let drop piece board =
    let (blocks, i, (x,y), r) = !piece in
    let add_to_board () =
        Array.iter (fun (px,py) ->
            !board.(x+px).(y+py)<-i;
        ) blocks; clear_lines board; false in
    let rec check remaining =
        match remaining with
        | [] -> piece := (blocks, i, (x,y-1), r); true
        | (px,py)::tl -> if check_point (x+px,y+py-1) board then check tl else add_to_board () in
    check (Array.to_list blocks)

let shift piece dir board =
    let (blocks, i, (x,y), r) = !piece in
    let rec check remaining =
        match remaining with
        | [] -> piece := (blocks, i, (x+dir,y), r)
        | (px,py)::tl -> if check_point(x+px+dir,y+py) board then check tl in
    check (Array.to_list blocks)

(* graphics *)
let draw_block i (x,y) s margin_side =
    let colors = [|red; green; blue; yellow; cyan; magenta; black|] in
    set_color colors.(i); fill_rect (s*(x+margin_side)) (s*y) (s-1) (s-1)

let draw_piece (blocks, i, (x,y), _) s margin_side =
    Array.iter (fun (px,py) -> draw_block i (x+px,y+py) s margin_side) blocks

let draw_frame width height margin_side scale color =
    set_color background; lineto (scale*margin_side) 0; set_color color;
    let lineto_scaled (x,y) = lineto (scale*x) (scale*y-1) in
    List.iter lineto_scaled [(margin_side,height); (margin_side+width,height); (margin_side+width,0)]

let draw_board board margin_side scale =
    let width = Array.length !board and height = Array.length !board.(0) in
    for x = 0 to width-1 do
        for y = 0 to height-1 do
            let i = !board.(x).(y) in
            if i != -1 then draw_block i (x,y) scale margin_side
        done
    done

let draw_next next width height s margin_side =
    let rec aux l i =
        match l with
        | [] -> ()
        | hd::tl ->
            draw_piece (pieces.(hd), hd, (width+2,height-i*4-2), 0) s margin_side;
            aux tl (i+1) in
    aux !next 0
(* graphics *)

let take_next next bag n_pieces n_next =
    let gen_bag n =
        shuffle (List.init n (fun x->x)) in
    let rec try_take () =
        match !bag with
        | [] -> bag := gen_bag n_pieces; try_take ()
        | hd::tl -> 
            match !next with
            | [] ->
                let rec take n from l =
                    match (n, !from) with
                    | (0, _) -> ()
                    | (_, []) -> ()
                    | (_, hd::tl) -> from := tl; l := hd::!l; take (n-1) from l in
                take n_next bag next; try_take ()
            | nhd::ntl -> next := ntl@[hd]; bag := tl; nhd in
    try_take ()

let () =
    Random.self_init ();
    let width = 10 and height = 24 and margin_side = 5 and margin_top = 2 and scale = 20 in
    let n_pieces = Array.length pieces and n_next = 4 in
    let make_piece i = (pieces.(i), i, (width/2,height), 0) in (* TODO: check spawn is clear, remove redundant first element *)

    let board = ref (Array.make_matrix width height (-1)) in
    let bag = ref [] and next = ref [] in
    let piece = ref (make_piece (take_next next bag n_pieces n_next)) in

    open_graph (sprintf (" %dx%d") (scale*(width+2*margin_side)) (scale*(height+margin_top)));
    let c = ref ' ' in
    while !c!='q' do
        clear_graph ();
        draw_board board margin_side scale;
        draw_next next width height scale margin_side;
        draw_piece !piece scale margin_side;
        draw_frame width height margin_side scale black;
        c := read_key ();
        match !c with
        | 'z' -> rotate piece (-1) board
        | 'x' -> rotate piece 1 board
        | 'm' -> shift piece (-1) board
        | ',' -> shift piece 1 board
        | _   -> if not (drop piece board) then piece := make_piece (take_next next bag n_pieces n_next)
    done
