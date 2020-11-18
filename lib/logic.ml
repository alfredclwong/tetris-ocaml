open Types

let gen_bag n_pieces =
  let shuffle arr =
    for i = (Array.length arr)-1 downto 1 do
      let j = Random.int (i+1) and tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    done;
    arr in
  let (--) a b = Array.init (b-a) (fun i -> a+i) in
  Array.to_list (shuffle (0--n_pieces))


let clear_lines board =
  let w = Array.length board and h = Array.length board.(0) in
  let clear_line y =
    for i = 0 to (w-1) do
      for j = y to (h-2) do
        board.(i).(j) <- board.(i).(j+1)
      done;
      board.(i).(h-1) <- (-1)
    done in
  let rec check x y =
    if y < h then (
      if x = w then (clear_line y; check 0 y)
      else if board.(x).(y) = (-1) then check 0 (y+1)
      else check (x+1) y) in
  check 0 0


let get_blocks piece_rots i r = List.nth (List.nth piece_rots i) r

let is_legal { board; _ }
             { width = w; height = h; piece_rots; _ }
             { i; loc = (x, y); r; _ } =
  let blocks = get_blocks piece_rots i r in
  let oob (bx, by) = (x+bx)<0 || (x+bx)>=w || (y+by)<0 || (y+by)>=h in
  let colliding (bx, by) = board.(x+bx).(y+by) > (-1) in
  let check_blocks f = List.for_all (fun b -> not (f b)) blocks in
  List.for_all check_blocks [oob; colliding]

let place state { piece_rots; _ } { i; loc = (x, y); r; _ } =
  let blocks = get_blocks piece_rots i r in
  List.iter (fun (bx, by) -> state.board.(x+bx).(y+by) <- i ) blocks;
  clear_lines state.board;
  {state with pip = None}


let pip_drops = function { loc = (x, y); _ } as pip ->
  List.init (y+1) (fun dy -> {pip with loc = (x, y-dy)})

let gen_pips { offsets; _ } = function
  | (Quit, _) | (Hold, _) | (Drop Soft, _) ->
    failwith "gen_states called for Quit/Hold/Drop Soft"
  | (Drop Hard, pip) -> pip_drops pip
  | (Rot dr, ({ i; loc = (x, y); r = r0; _ } as pip)) ->
    let r1 = (r0 + if dr = CCW then 3 else 5) mod 4 in
    let rot (x0, y0) (x1, y1) = {pip with loc = (x+x0-x1, y+y0-y1); r = r1} in
    Array.to_list (Array.map2 rot offsets.(i).(r0) offsets.(i).(r1))
  | (Shift dx, ({ loc = (x0, y); _ } as pip)) ->
    let x1 = x0 + if dx = L then (-1) else 1 in
    [{pip with loc = (x1, y)}]

let update_ghost state cfg pip =
  let pips = pip_drops pip in
  let is_pip_illegal pip = not (is_legal state cfg pip) in
  let illegal_pip = List.find_opt is_pip_illegal pips in
  match illegal_pip with
  | None -> {pip with ghost = 0}
  | Some { loc = (_, y); _ } -> {pip with ghost = y+1}


let spawn state = function { height = h; piece_rots; _ } as cfg ->
  let (i, queue, bag) =
    let n = List.length piece_rots in
    match (state.queue, state.bag) with (* hard coded q 4 *)
    | (i::a::b::c::[d], bag) -> (i, [a;b;c;d], bag)
    | (i::tl, ([_] as a)) -> (i, tl@a, gen_bag n)
    | (i::tl, hd::bag) -> (i, tl@[hd], bag)
    | ([] ,[]) -> (
      match gen_bag n with
      | i::a::b::c::d::bag -> (i, [a;b;c;d], bag)
      | _ -> failwith "gen_bag n produced <5 elements")
    | ([], _) | (_, []) -> failwith "empty bag xor empty queue" in
  let spawn_pip = update_ghost state cfg { i; loc = (5, h-2); r = 0; ghost = 0 } in
  let spawn_state = {state with pip = Some spawn_pip; queue; bag} in
  if is_legal state cfg spawn_pip then Some spawn_state else None


let soft_drop state cfg = function { loc = (x, y); _ } as pip ->
  let dropped_pip = {pip with loc = (x, y-1)} in
  if is_legal state cfg dropped_pip then
    Some {state with pip = Some dropped_pip}
  else
    spawn (place state cfg pip) cfg

let hard_drop state cfg = function { loc = (x, _); ghost = y; _ } as pip ->
  spawn (place state cfg {pip with loc = (x, y)}) cfg

let hold state cfg i =
  let state = {state with pip = None} in
  let state = match state.hold with
  | None   -> {state with hold = Some i}
  | Some j -> {state with hold = Some i; queue = j::state.queue} in
  spawn state cfg


let tick move state cfg =
  match (move, state.pip) with
  | (Some Quit, _) -> None
  | (_, None) -> spawn state cfg
  | (None, _) -> Some state
  | (Some Hold, Some {i; _}) -> hold state cfg i
  | (Some Drop Soft, Some pip) -> soft_drop state cfg pip
  | (Some Drop Hard, Some pip) -> hard_drop state cfg pip
  | (Some move, Some pip) ->
    let is_legal pip = is_legal state cfg pip in
    match List.find_opt is_legal (gen_pips cfg (move, pip)) with
    | None -> Some state (* tick 's' state cfg *) (* illegal move: soft drop or pass? *)
    | Some pip -> Some {state with pip = Some (update_ghost state cfg pip)}
