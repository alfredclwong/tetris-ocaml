type piece_in_play = { id: int; loc: int * int; rot: int }
type state = { score: int; board: int array array; pip: piece_in_play option; hold: int option; queue: int list; bag: int list }
type shift = L | R
type rot = CCW | CW
type drop = Fall | Soft | Hard
type move = Shift of shift | Rot of rot | Drop of drop | Hold | Quit

let (--) a b = Array.init (b-a) (fun i -> a+i)

let gen_bag n =
    let shuffle a =
        for i = (Array.length a)-1 downto 1 do
            let j = Random.int (i+1) in
            let tmp = a.(i) in
            a.(i) <- a.(j);
            a.(j) <- tmp
        done;
        a in
    Array.to_list (shuffle (0--n))

let is_legal { board = b; pip = p } = function
    | Drop  d -> true
    | Rot   r -> true
    | Shift s -> true
    | Hold    -> true
    | Quit    -> failwith "something is checking if Quit is legal"

let legal_moves board pip = [Quit]

let draw { score = s; board = b; pip = p; hold = h; queue = q; bag } =
    let open Graphics in
    let draw_score = () in
    let draw_board = () in
    let draw_pip = () in
    let draw_hold = () in
    let draw_queue = () in
    let draw_grid = () in
    draw_score; draw_board; draw_pip; draw_hold; draw_queue; draw_grid

let init_state =
    let w = 10 and h = 24 and n = 7 in
    let board = Array.make_matrix w h (-1) in
    let bag = gen_bag n in
    { score = 0; board = board; pip = None; hold = None; queue = []; bag = bag }

let rec tick_state state = function
    | Quit -> None
    | Drop Fall as move -> if is_legal state move then Some state else None
    | move -> if is_legal state move then Some state else tick_state state (Drop Fall)

let tetris =
    Random.self_init ();
    Graphics.open_graph " 10x10";
    let rec loop = function
        | (state::_, _) as history -> (
            draw state;
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
        | ([], _) -> loop ([init_state], []) in
    loop ([], [])

let () =
    let history = tetris in
    Printf.printf "%d\n" (List.length (fst history))
