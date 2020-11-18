open Types

let is_legal board
             { width = w; height = h; piece_rots; _ }
             { i; loc = (x, y); r; _ } =
  let blocks = List.nth (List.nth piece_rots i) r in
  let oob (bx, by) = (x+bx)<0 || (x+bx)>=w || (y+by)<0 || (y+by)>=h in
  let colliding (bx, by) = board.(x+bx).(y+by) > (-1) in
  let check_blocks f = List.for_all (fun b -> not (f b)) blocks in
  List.for_all check_blocks [oob; colliding]

let gen_pips { offsets; _ } ({ i; loc = (x, y); r = r0; _ } as pip) = function
  | Quit | Hold | Drop Soft -> failwith "gen_pips called on invalid move"
  | Drop Hard -> List.init (y+1) (fun dy -> {pip with loc = (x, y-dy)})
  | Shift dx -> [{pip with loc = ((x + if dx = L then (-1) else 1), y)}]
  | Rot dr ->
    let r1 = (r0 + if dr = CCW then 3 else 5) mod 4 in
    let rot (x0, y0) (x1, y1) = {pip with loc = (x+x0-x1, y+y0-y1); r = r1} in
    Array.to_list (Array.map2 rot offsets.(i).(r0) offsets.(i).(r1))

let update_ghost board cfg pip =
  let pips = gen_pips cfg pip (Drop Hard) in
  let is_pip_illegal pip = not (is_legal board cfg pip) in
  let illegal_pip = List.find_opt is_pip_illegal pips in
  match illegal_pip with
  | None                     -> {pip with ghost = 0}
  | Some { loc = (_, y); _ } -> {pip with ghost = y+1}

let spawn ({ board; queue; bag; _ } as state)
          ({ height = h; piece_rots; _ } as cfg) =

  let gen_bag n_pieces =
    let shuffle arr =
      for i = (Array.length arr)-1 downto 1 do
        let j = Random.int (i+1) and tmp = arr.(i) in
        arr.(i) <- arr.(j);
        arr.(j) <- tmp
      done;
      arr in
    let (--) a b = Array.init (b-a) (fun i -> a+i) in
    Array.to_list (shuffle (0--n_pieces)) in

  let (i, queue, bag) =
    let n = List.length piece_rots in
    match (queue, bag) with (* hard coded q 4 *)
    | (i::a::b::c::[d], bag) -> (i, [a;b;c;d], bag)
    | (i::tl, ([_] as a))    -> (i, tl@a, gen_bag n)
    | (i::tl, hd::bag)       -> (i, tl@[hd], bag)
    | ([] ,[]) -> (
      match gen_bag n with
      | i::a::b::c::d::bag   -> (i, [a;b;c;d], bag)
      | _                    -> failwith "gen_bag n produced <5 elements")
    | ([], _) | (_, [])      -> failwith "empty bag xor empty queue" in

  let loc = (5, h-2) and r = 0 and ghost = 0 in
  let pip = update_ghost board cfg { i; loc; r; ghost } in
  let state = {state with pip = Some pip; queue; bag} in
  if is_legal board cfg pip then Some state else None

let drop ({ board; _ } as state)
         ({ width = w; height = h; piece_rots; _ } as cfg)
         ({ loc = (x, y); ghost = g; _ } as pip) =

  let clear_lines board =
    let clear_line y =
      for i = 0 to (w-1) do for j = y to (h-2) do
        board.(i).(j) <- board.(i).(j+1) done;
        board.(i).(h-1) <- (-1) done in
    let rec check x y =
      if y < h then (
        if x = w then (clear_line y; check 0 y)
        else if board.(x).(y) = (-1) then check 0 (y+1)
        else check (x+1) y) in
    check 0 0 in

  let set_pip ({ board; _ } as state) { i; loc = (x, y); r; _ } =
    let blocks = List.nth (List.nth piece_rots i) r in
    let update_board (bx, by) = board.(x+bx).(y+by) <- i in
    List.iter update_board blocks; clear_lines board;
    {state with pip = None} in

  function
  | Soft ->
    let dropped_pip = {pip with loc = (x, y-1)} in
    if is_legal board cfg dropped_pip then
      Some {state with pip = Some dropped_pip}
    else spawn (set_pip state pip) cfg
  | Hard ->
    spawn (set_pip state {pip with loc = (x, g)}) cfg

let hold ({ hold; queue; _ } as state) cfg i =
  let state = {state with pip = None} in
  let state = match hold with
  | None   -> {state with hold = Some i}
  | Some j -> {state with hold = Some i; queue = j::queue} in
  spawn state cfg


let tick ({ board; pip; _ } as state) cfg move =
  match (move, pip) with
  | (Some Quit, _)             -> None
  | (_, None)                  -> spawn state cfg (* any key starts game *)
  | (None, _)                  -> Some state
  | (Some Drop d, Some pip)    -> drop state cfg pip d
  | (Some Hold, Some { i; _ }) -> hold state cfg i
  | (Some move, Some pip)      ->
    let is_legal pip = is_legal board cfg pip in
    match List.find_opt is_legal (gen_pips cfg pip move) with
    | None -> Some state (* illegal move: soft drop or pass? *)
    | Some pip -> Some {state with pip = Some (update_ghost board cfg pip)}
