type piece_in_play = { i: int; loc: int * int; r: int }
type state = { score: int; board: int array array; pip: piece_in_play option; hold: int option; queue: int list; bag: int list }
type shift = L | R
type rot = CCW | CW
type drop = Fall | Soft | Hard
type move = Shift of shift | Rot of rot | Drop of drop | Hold | Quit
type cfg = { width: int; height: int; piece_rots: (int * int) list list list; offsets: (int * int) list list list;
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

let is_legal { board = b; pip = p } = function
    | Drop  d -> true
    | Rot   r -> true
    | Shift s -> true
    | Hold    -> true
    | Quit    -> failwith "something is checking if Quit is legal"

let legal_moves board pip = [Quit]

let init_state w h n =
    let board = Array.make_matrix w h (-1) in
    let rec transfer n from l =
        match (n, from) with
        | (0, _) | (_, []) -> (from, l)
        | (_, hd::tl) -> transfer (n-1) tl (hd::l) in
    let (bag, queue) = transfer 4 (gen_bag n) [] in
    { score = 0; board = board; pip = None; hold = None; queue = queue; bag = bag }

let rec tick_state state = function
    | Quit -> None
    | Drop Fall as move -> if is_legal state move then Some state else None
    | move -> if is_legal state move then Some state else tick_state state (Drop Fall)

let draw { score; board = b; pip; hold; queue = q; bag } { width = w; height = h; scale = s; margin_side = ms; colors; piece_rots } =
    let open Graphics in
    let draw_line x0 y0 x1 y1 c = moveto (s*x0) (s*y0); set_color c; lineto (s*x1) (s*y1) in
    let draw_block x y c = set_color c; fill_rect (s*x) (s*y) (s-1) (s-1) in
    let draw_blocks x y c blocks = List.iter (fun (bx, by) -> draw_block (x+bx) (y+by) c) blocks in
    let draw_text ?(c=black) x y t = moveto (s*x) (s*y); set_color c; draw_string t in
    let draw_score x y =
        List.iter2 (fun (x, y) t -> draw_text x y t)
        [ (x, y); (x, y-1) ] [ "SCORE"; (Printf.sprintf "%d" score) ] in
    let draw_board =
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
    let draw_grid =
        let rows = List.init (h-1) (fun i -> (0, (i+1), w, (i+1))) in
        let cols = List.init (w-1) (fun i -> ((i+1), 0, (i+1), h)) in
        let frame = [ (0, 0, 0, h); (0, h, w, h); (w, h, w, 0) ] in
        let gray = rgb 224 224 224 in
        List.iter (fun (c, lines) ->
            List.iter (fun (x0, y0, x1, y1) ->
                draw_line (ms+x0) y0 (ms+x1) y1 c)
            lines)
        [(gray, rows); (gray, cols); (black, frame)] in
    draw_score (ms+w+1) (h-15);
    draw_queue (ms+w+1) (h-1) 0 q;
    draw_hold 1 (h-1) 0 hold;
    draw_board; draw_pip pip; draw_grid

let tetris = function { width = w; height = h; piece_rots; offsets; scale = s;
                        margin_side = ms; margin_top = mt; colors } as cfg ->
        Random.self_init ();
        Graphics.open_graph (Printf.sprintf " %dx%d" ((w+ms*2)*s) ((h+mt)*s));
        let rec loop = function
            | (state::_, _) as history -> (
                draw state cfg;
                let move =
                    match Graphics.read_key () with
                    | 'q' -> Quit
                    | 'j' -> Rot CCW
                    | 'k' -> Rot CW
                    | 'l' -> Hold
                    | 'w' -> Drop Hard
                    | 'a' -> Shift L
                    | 's' -> Drop Soft
                    | 'd' -> Shift R
                    | _   -> Drop Fall in
                match tick_state state move with
                | None -> history
                | Some next_state ->
                    let (state_history, move_history) = history in
                    loop (next_state :: state_history, move :: move_history))
            | ([], _) -> loop ([init_state w h (List.length piece_rots)], []) in
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
            [[( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0)];
             [( 0, 0); ( 1, 0); ( 1,-1); ( 0, 2); ( 1, 2)];
             [( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0)];
             [( 0, 0); (-1, 0); (-1,-1); ( 0, 2); (-1, 2)]] and
        i = [[( 0, 0); (-1, 0); ( 2, 0); (-1, 0); ( 2, 0)];
             [(-1, 0); ( 0, 0); ( 0, 0); ( 0, 1); ( 0,-2)];
             [(-1, 1); ( 1, 1); (-2, 1); ( 1, 0); (-2, 0)];
             [( 0, 1); ( 0, 1); ( 0, 1); ( 0,-1); ( 0, 2)]] and
        o = [[( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0); ( 0, 0)];
             [( 0,-1); ( 0,-1); ( 0,-1); ( 0,-1); ( 0,-1)];
             [(-1,-1); (-1,-1); (-1,-1); (-1,-1); (-1,-1)];
             [(-1, 0); (-1, 0); (-1, 0); (-1, 0); (-1, 0)]] in
        let z = j and l = j and t = j and s = j in [j;i;z;l;o;t;s] in

    let open Graphics in
    let orange = rgb 255 153 51 and purple = rgb 153 51 255 in
    let colors = [| blue; cyan; red; orange; yellow; purple; green |] in

    let cfg = { width = 10; height = 24; piece_rots = piece_rots; offsets = offsets;
                scale = 20; margin_side = 5; margin_top = 2; colors = colors } in
    let history = tetris cfg in
    Printf.printf "%d\n" (List.length (fst history))
