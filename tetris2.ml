type shift = L | R
type rot = CCW | CW
type drop = Soft | Hard
type move = Shift of shift | Rot of rot | Drop of drop | Hold | Quit
type piece_in_play = { i: int; loc: int * int; r: int }
type state = { score: int; board: int array array; pip: piece_in_play option; hold: int option; queue: int list; bag: int list }
type cfg = { width: int; height: int; piece_rots: (int * int) list list list; offsets: (int * int) array array array;
             scale: int; margin_side: int; margin_top: int; colors: Graphics.color array }

let (--) a b = Array.init (b-a) (fun i -> a+i)

let gen_bag n_pieces =
    let shuffle a =
        for i = (Array.length a)-1 downto 1 do
            let j = Random.int (i+1) in
            let tmp = a.(i) in
            a.(i) <- a.(j);
            a.(j) <- tmp
        done;
        a in
    Array.to_list (shuffle (0--n_pieces))

let is_legal { board; pip } { width = w; height = h; piece_rots } =
    match pip with
    | None -> true
    | Some { i; loc = (x, y); r } ->
        let blocks = List.nth (List.nth piece_rots i) r in
        let oob (bx, by) = (x+bx)<0 || (x+bx)>=w || (y+by)<0 || (y+by)>=h in
        let colliding (bx, by) = board.(x+bx).(y+by) > (-1) in
        let check_blocks f = List.for_all (fun b -> not (f b)) blocks in
        List.for_all check_blocks [oob; colliding]

let clear_lines board =
    let w = Array.length board and h = Array.length board.(0) in
    let clear_line y =
        for i = 0 to (w-1) do
            for j = y to (h-2) do
                board.(i).(j) <- board.(i).(j+1) done;
            board.(i).(h-1) <- (-1);
        done in
    let rec check x y =
        if y < h then (
            if x = w then (clear_line y; check 0 y)
            else if board.(x).(y) = (-1) then check 0 (y+1)
            else check (x+1) y) in
    check 0 0

let rec tick c state cfg =
    let move =
        match c with
        | 'q' -> Some (Quit)
        | 'j' -> Some (Rot CCW)
        | 'k' -> Some (Rot CW)
        | 'l' -> Some (Hold)
        | 'w' -> Some (Drop Hard)
        | 'a' -> Some (Shift L)
        | 's' -> Some (Drop Soft)
        | 'd' -> Some (Shift R)
        | _   -> None in
    let gen_pips = function
        | (Quit, _) | (Hold, _) | (Drop Soft, _) ->
            failwith "gen_states called for Quit/Hold/Drop Soft"
        | (Drop Hard, ({ loc = (x, y); _ } as pip)) ->
            List.init (y+1) (fun dy -> {pip with loc = (x, y-dy)})
        | (Rot dr, ({ i; loc = (x, y); r = r0 } as pip)) ->
            let r1 = (r0 + if dr = CCW then 3 else 5) mod 4 in
            let rot (x0, y0) (x1, y1) = {pip with loc = (x+x0-x1, y+y0-y1); r = r1} in
            Array.to_list (Array.map2 rot cfg.offsets.(i).(r0) cfg.offsets.(i).(r1))
        | (Shift dx, ({ loc = (x0, y); _ } as pip)) ->
            [{pip with loc = (x0 + (if dx = L then (-1) else 1), y)}] in
    let is_pip_legal pip = is_legal {state with pip = Some pip} cfg in
    let place state { i; loc = (x, y); r } =
        let blocks = (List.nth (List.nth cfg.piece_rots i) r) in
        List.iter (fun (bx, by) -> state.board.(x+bx).(y+by) <- i ) blocks;
        clear_lines state.board; {state with pip = None} in
    let pass state = tick ' ' state cfg in
    let soft_drop = function { loc = (x, y); _ } as pip ->
        let dropped_state = {state with pip = Some {pip with loc = (x, y-1)}} in
        if is_legal dropped_state cfg then Some dropped_state
        else pass (place state pip) in
    let hard_drop = function { loc = (x, y) } as pip ->
        let pips = gen_pips (Drop Hard, pip) in
        let is_pip_illegal pip = not (is_pip_legal pip) in
        let illegal_pip = List.find_opt is_pip_illegal pips in
        let next_pip = match illegal_pip with
            | None -> {pip with loc = (x, 0)}
            | Some { loc = (x, y) } -> {pip with loc = (x, y+1)} in
        pass (place state next_pip) in
    let hold i =
        let state = {state with pip = None} in
        match state.hold with
        | None -> pass {state with hold = Some i}
        | Some j -> pass {state with hold = Some i; queue = j::state.queue} in
    let spawn =
        let (i, queue, bag) =
            let n = List.length cfg.piece_rots in
            match (state.queue, state.bag) with (* hard coded q 4 *)
            | (i::a::b::c::[d], bag) -> (i, [a;b;c;d], bag)
            | (i::tl, ([hd] as a)) -> (i, tl@a, gen_bag n)
            | (i::tl, hd::bag) -> (i, tl@[hd], bag)
            | ([] ,[]) -> (
                match gen_bag n with
                | i::a::b::c::d::bag -> (i, [a;b;c;d], bag)
                | _ -> failwith "gen_bag n produced <5 elements")
            | ([], _) | (_, []) -> failwith "empty bag xor empty queue" in
        let spawn_pip = { i; loc = (5, cfg.height-2); r = 0 } in
        let spawn_state = {state with pip = Some spawn_pip; queue; bag} in
        if is_legal spawn_state cfg then Some spawn_state else None in
    match (move, state.pip) with
    | (Some Quit, _) -> None
    | (_, None) -> spawn
    | (None, _) -> Some state
    | (Some Hold, Some {i}) -> hold i
    | (Some Drop Soft, Some pip) -> soft_drop pip
    | (Some Drop Hard, Some pip) -> hard_drop pip
    | (Some move, Some pip) ->
        match List.find_opt is_pip_legal (gen_pips (move, pip)) with
        | None -> Some state (* tick 's' state cfg *) (* soft drop or pass? *)
        | pip -> Some {state with pip}

let draw { score; board = b; pip; hold; queue = q; bag } { width = w; height = h; scale = s; margin_side = ms; colors; piece_rots } =
    let open Graphics in
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
        | Some {i; loc = (x, y); r} ->
            let blocks = List.nth (List.nth piece_rots i) r in
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
        let gray = rgb 224 224 224 in
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

let tetris = function { width = w; height = h; piece_rots; offsets; scale = s;
                        margin_side = ms; margin_top = mt; colors } as cfg ->
    Random.self_init ();
    Graphics.open_graph (Printf.sprintf " %dx%d" ((w+ms*2)*s) ((h+mt)*s));
    let rec loop = function
        | ((curr_state::_ as state_history), move_history) as history -> (
            draw curr_state cfg;
            match tick (Graphics.read_key ()) curr_state cfg with
            | None -> history
            | Some next_state -> loop (next_state::state_history, move_history))
        | ([], _) ->
            let score = 0 and board = Array.make_matrix w h (-1) and
                pip = None and hold = None and queue = [] and bag = [] in
            let state0 = {score; board; pip; hold; queue; bag} in
            loop ([state0], []) in
    loop ([], [])

let () =
    let piece_rots =
        let pieces =
            [[(-1, 0); (-1, 1); ( 0, 0); ( 1, 0)];
             [(-1, 0); ( 0, 0); ( 1, 0); ( 2, 0)];
             [(-1, 1); ( 0, 0); ( 0, 1); ( 1, 0)];
             [(-1, 0); ( 0, 0); ( 1, 0); ( 1, 1)];
             [( 0, 0); ( 0, 1); ( 1, 0); ( 1, 1)];
             [(-1, 0); ( 0, 0); ( 0, 1); ( 1, 0)];
             [(-1, 0); ( 0, 0); ( 0, 1); ( 1, 1)]] in
        let rec gen_rots = function
            | (0, rots) -> List.rev rots
            | (n, (hd::_ as rots))  ->
                let rotate = function (x, y) -> (y, -x) in
                gen_rots ((n-1), List.map rotate hd :: rots)
            | (_, []) -> failwith "gen_rots called on empty list" in
        List.map (fun piece -> gen_rots (3, [piece])) pieces in
    let offsets =
        let j =
            [|[|( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0)|];
              [|( 0, 0); ( 1, 0); ( 1,-1); ( 0, 2); ( 1, 2)|];
              [|( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0)|];
              [|( 0, 0); (-1, 0); (-1,-1); ( 0, 2); (-1, 2)|]|] and
        i = [|[|( 0, 0); (-1, 0); ( 2, 0); (-1, 0); ( 2, 0)|];
              [|(-1, 0); ( 0, 0); ( 0, 0); ( 0, 1); ( 0,-2)|];
              [|(-1, 1); ( 1, 1); (-2, 1); ( 1, 0); (-2, 0)|];
              [|( 0, 1); ( 0, 1); ( 0, 1); ( 0,-1); ( 0, 2)|]|] and
        o = Array.map (fun xy -> Array.make 5 xy)
              [|( 0, 0); ( 0,-1); (-1,-1); (-1, 0)|] in
        let z = j and l = j and t = j and s = j in [|j;i;z;l;o;t;s|] in

    let open Graphics in
    let orange = rgb 255 153 51 and purple = rgb 153 51 255 in
    let colors = [| blue; cyan; red; orange; yellow; purple; green |] in

    let cfg = { width = 10; height = 24; piece_rots = piece_rots; offsets = offsets;
                scale = 20; margin_side = 5; margin_top = 2; colors = colors } in
    let history = tetris cfg in
    Printf.printf "%d\n" (List.length (fst history))
