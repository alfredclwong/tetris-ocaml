open Printf
open Graphics

let width = 10 and height = 24
let scale n = n * 20
let piece_error name = sprintf "Piece %c not recognised." name
let pieces name =
    match name with
    | 'J' -> ([(-1, 0); (-1, 1); ( 0, 0); ( 1, 0)], red    )
    | 'I' -> ([(-1, 0); ( 0, 0); ( 1, 0); ( 2, 0)], green  )
    | 'Z' -> ([(-1, 1); ( 0, 0); ( 0, 1); ( 1, 0)], blue   )
    | 'L' -> ([(-1, 0); ( 0, 0); ( 1, 0); ( 1, 1)], yellow )
    | 'O' -> ([( 0, 0); ( 0, 1); ( 1, 0); ( 1, 1)], cyan   )
    | 'T' -> ([(-1, 0); ( 0, 0); ( 0, 1); ( 1, 0)], magenta)
    | 'S' -> ([(-1, 0); ( 0, 0); ( 0, 1); ( 1, 1)], black  )
    | _   -> failwith (piece_error name)

let draw name x y =
    let (piece, color) = pieces name and s = scale 1 in
    set_color color;
    List.iter (fun (px, py) -> fill_rect (scale (x+px)) (scale (y+py)) s s) piece

let rec zip two_lists =
    match two_lists with
    | [], [] -> []
    | h1::t1, h2::t2 -> (h1, h2)::(zip (t1, t2))
    | _, _ -> failwith "tried to zip two lists with different lengths"

let (--) a b =
    let rec aux i acc =
        if i < a then acc else aux (i-1) (i :: acc)
    in aux (b-1) []

let () =
    open_graph (sprintf (" %dx%d") (scale width) (scale height));
    let s = "JIZLOTS" in
    let n = String.length s in
    let ys = List.map (fun x -> x*3+1) (0--n) in
    let names = List.init n (String.get s) in
    List.iter (fun (name, y) -> draw name (width/2) y) (zip (names, ys))
let c = read_key ()
