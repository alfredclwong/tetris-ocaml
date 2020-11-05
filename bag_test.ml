open Printf
let () =
    Random.self_init ();
    let shuffle d =
        let nd = List.map (fun c -> (Random.bits (), c)) d in
        let sond = List.sort compare nd in
        List.map snd sond in
    let gen_bag n =
        shuffle (List.init n (fun x->x)) in
    let take_next next bag n_pieces n_next =
        let rec aux () =
            match !bag with
            | [] -> bag := gen_bag n_pieces; aux ()
            | hd::tl -> 
                match !next with
                | [] ->
                    let rec take n from l =
                        match (n, !from) with
                        | (0, _) -> ()
                        | (_, []) -> ()
                        | (_, hd::tl) -> from := tl; l := hd::!l; take (n-1) from l in
                    take n_next bag next; aux ()
                | nhd::ntl -> next := ntl@[hd]; bag := tl; nhd in
        aux () in
    let next = ref [] and bag = ref [] in
    for i = 1 to 14 do
        printf "%d " (take_next next bag 7 4);
        print_endline "";
        List.iter (printf "%d ") !bag;
        print_endline "";
        List.iter (printf "%d ") !next;
        print_endline ""
    done;
    print_endline ""
