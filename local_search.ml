open Tools;;

exception Unreachable;;

type dist = int;;
type 'a problem = {
    (** generate a random state from a random int *)
    orig_f: (unit -> int) -> 'a;
    (** generate a list of next states for state *)
    next_f: 'a -> 'a list;
    (** how close the state is to the goal
        (0 means goal reached) *)
    val_f: 'a -> dist;
    (** representation of the state as string *)
    to_string: 'a -> string
};;

let hill_climbing_search { orig_f; next_f; val_f; to_string } =
    let add_val state =
        (val_f state, state)
    in
    (* generate *beam size* random origins *)
    let init_state () =
        let gen_random_int () =
            Random.full_int (Int.max_int)
        in
        gen_random_int
        |> orig_f
        |> add_val
    in
    let rec search' (state_val, state) =
        if state_val = 0 then state
        else
            (* generate state's neighbours *)
            let best_next state =
                next_f state
                |> List.map add_val
                |> List.sort (fun (v1, _) (v2, _) -> v1 - v2)
                |> (function [] -> None | hd :: _ -> Some hd)
            in

            match best_next state with
            (* next state is better than the current - continue search *)
            | Some ((next_val, _) as next) when next_val < state_val ->
                    search' next
            (* goal not found -> restart *)
            | _ -> search' (init_state ())
    in
    init_state ()
    |> search'
;;

Random.self_init ();;

(* beam local search *)
let beam_search beam_size { orig_f; next_f; val_f; to_string } =
    let add_val state =
        (val_f state, state)
    in
    (* generate *beam size* random origins *)
    let init_beam beam_size =
        let gen_random_int () =
            Random.full_int (Int.max_int)
        in
        let gen_random_list beam_size =
            let rec gen' acc = function
            | 0 -> acc
            | n -> gen' (orig_f gen_random_int :: acc) (n - 1)
            in
            gen' [] beam_size
        in
        gen_random_list beam_size
        |> List.map add_val
    in
    (* execute beam search *)
    let rec search' visited beam =
        (* generate neighbourhood to the beam *)
        let gen_neigh beam =
            let gen_neigh' neigh (_, state) =
                let filter_visited state =
                    let val_state = add_val state in
                    if List.mem val_state visited then None
                    else Some val_state
                in
                List.filter_map filter_visited (next_f state) @ neigh
            in
            List.fold_left gen_neigh' [] beam
        (* filter the neighbourhood into a new beam *)
        and filter_best states =
            let rec trim len = function
                | [] -> []
                | _ :: _ when len = 0 -> []
                | hd :: tl -> hd :: trim (len - 1) tl
            in
            states
            |> List.sort (fun (v1, _) (v2, _) -> v1 - v2)
            |> trim beam_size
        in

        let (best_val, best) = List.hd beam in
        if best_val = 0 then best
        else
            let neighbours = gen_neigh beam in
            let next_beam = filter_best neighbours in
            match List.nth_opt next_beam 0 with
            (* next state is better than the current - continue search *)
            | Some (best_next_val, best_next) when best_next_val < best_val ->
                    search' (neighbours @ visited) next_beam
            (* goal not found -> restart *)
            | _ -> search' [] (init_beam beam_size)
    in
    init_beam beam_size
    |> search' []
;;

let labirinth = {
    orig_f = (function rand ->
        match rand () mod 4 with
        | 0 -> [|1; 0; 0; 0|]
        | 1 -> [|0; 1; 0; 0|]
        | 2 -> [|0; 0; 1; 0|]
        | _ -> [|0; 0; 0; 1|]
    );
    next_f = (function
        | [|1; 0; 0; 0|] -> [ [|0; 1; 0; 0|] ]
        | [|0; 1; 0; 0|] -> [ [|1; 0; 0; 0|]; [|0; 0; 1; 0|] ]
        | [|0; 0; 1; 0|] -> [ [|0; 1; 0; 0|]; [|0; 0; 0; 1|] ]
        | [|0; 0; 0; 1|] -> [ [|0; 0; 1; 0|]; [|0; 0; 0; 0|] ]
        | [|0; 0; 0; 0|] -> [ [|0; 0; 0; 1|] ]
        | _ -> raise Unreachable
    );
    val_f = (function
        | [|1; 0; 0; 0|] -> 4
        | [|0; 1; 0; 0|] -> 3
        | [|0; 0; 1; 0|] -> 2
        | [|0; 0; 0; 1|] -> 1
        | [|0; 0; 0; 0|] -> 0
        | _ -> raise Unreachable
    );
    to_string = (function
        | [|a; b; c; d|] -> Format.sprintf "[|%d %d %d %d|]" a b c d
        | _ -> raise Unreachable
    )
}
;;

assert (hill_climbing_search labirinth = [|0; 0; 0; 0|]);;
assert (beam_search 2 labirinth = [|0; 0; 0; 0|]);;

let eight_queens = {
    orig_f = (function rand ->
        let rec rand_pos row =
            (row, rand () mod 8)
        in
        Array.init 8 rand_pos
    );
    next_f = (function state ->
        let queen_neighbours = function
            | (a, 0) -> [(a, 1)]
            | (a, 7) -> [(a, 6)]
            | (a, b) -> [(a, b - 1); (a, b + 1)]
        and move_queen state i neigh =
            let copy = Array.copy state in
            copy.(i) <- neigh;
            copy
        in
        let add_states states i neighs =
            List.map (move_queen state i) neighs
        in
        Array.map queen_neighbours state
        |> fold_lefti add_states []
    );
    val_f = (function state ->
        let attacks i j =
            if i = j then 0
            else
                let (y1, x1) = state.(i)
                and (y2, x2) = state.(j) in
                if x1 = x2
                || y1 = y2
                || Int.abs (x1 - x2) = Int.abs (y1 - y2)
                then 1
                else 0
        in
        fold_lefti
            (fun acc i _ ->
                acc + fold_lefti (fun acc j _ ->
                    acc + attacks i j) 0 state)
            0 state
    );
    to_string = (function state ->
        let add_row acc queen_pos =
            acc ^
            match queen_pos with
            | (_, 0) -> "Q.......\n"
            | (_, 1) -> ".Q......\n"
            | (_, 2) -> "..Q.....\n"
            | (_, 3) -> "...Q....\n"
            | (_, 4) -> "....Q...\n"
            | (_, 5) -> ".....Q..\n"
            | (_, 6) -> "......Q.\n"
            | (_, _) -> ".......Q\n"
        in
        Array.fold_left add_row "" state
    )
}
;;

print_endline
@@ eight_queens.to_string
@@ hill_climbing_search eight_queens

