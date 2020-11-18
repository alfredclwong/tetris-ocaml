open Graphics
let orange = rgb 255 153 51 and purple = rgb 153 51 255
let colors = [| blue; cyan; red; orange; yellow; purple; green |]

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
  List.map (fun piece -> gen_rots (3, [piece])) pieces

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
  let z = j and l = j and t = j and s = j in
  [|j;i;z;l;o;t;s|]
