open Tools
open Local_search

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
        fold_lefti (fun acc i _ ->
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

let open Format in
let (queens, restarts) = beam_search 14 eight_queens in
print_endline
@@ eight_queens.to_string
@@ queens;
printf "restarts: %d\n" restarts

