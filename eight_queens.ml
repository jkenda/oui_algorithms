open Tools
open Local_search

let eight_queens = {
    orig_f = (function rand ->
        let rec rand_col row =
            rand () mod 8
        in
        Array.init 8 rand_col
    );
    next_f = (function state ->
        let queen_neighbours col =
            let rec gen' acc = function
                | 8 -> acc
                | x -> gen' (if x = col then acc else x :: acc) (x + 1)
            in gen' [] 0
        and move_queen state row neigh =
            let copy = Array.copy state in
            copy.(row) <- neigh;
            copy
        in
        let add_states states row neigh =
            List.map (move_queen state row) neigh @ states
        in
        Array.map queen_neighbours state
        |> fold_lefti add_states []
    );
    val_f = (function state ->
        let attacks i j =
            if i = j then 0
            else
                let open Int in
                let (y1, x1) = i, state.(i)
                and (y2, x2) = j, state.(j) in
                if x1 = x2 || y1 = y2
                || abs (x1 - x2) = abs (y1 - y2)
                then 1
                else 0
        in
        fold_lefti (fun acc i _ ->
            acc + fold_lefti (fun acc j _ ->
                acc + attacks i j) 0 state)
            0 state
    );
    to_string = (function state ->
        let add_row acc = function
            | 0 -> acc ^ "Q . . . . . . .\n"
            | 1 -> acc ^ ". Q . . . . . .\n"
            | 2 -> acc ^ ". . Q . . . . .\n"
            | 3 -> acc ^ ". . . Q . . . .\n"
            | 4 -> acc ^ ". . . . Q . . .\n"
            | 5 -> acc ^ ". . . . . Q . .\n"
            | 6 -> acc ^ ". . . . . . Q .\n"
            | _ -> acc ^ ". . . . . . . Q\n"
        in
        Array.fold_left add_row "" state
    )
}
;;

let open Format in
let (queens, restarts) = beam_search 8 eight_queens in
print_endline
@@ eight_queens.to_string
@@ queens;
printf "restarts: %d\n" restarts

