open Tools
open Local_search

let eight_queens = {
    orig_f = (function rand ->
        let rand_row _ =
            rand () mod 8
        in
        Array.init 8 rand_row
    );
    next_f = (function rows ->
        let neigh col =
            let rec gen' acc = function
                | 8 -> acc
                | x -> gen' (if x = col then acc else x :: acc) (x + 1)
            in gen' [] 0
        and add_neighs states row neighs =
            let move state row neigh =
                let copy = Array.copy state in
                copy.(row) <- neigh;
                copy
            in
            List.map (move rows row) neighs @ states
        in
        Array.map neigh rows
        |> fold_lefti add_neighs []
    );
    dist_f = (function rows ->
        let attacking i j =
            let open Int in
            if i = j then 0
            else
                let x1 = rows.(i)
                and x2 = rows.(j) in
                if x1 = x2 || abs (x1 - x2) = abs (i - j)
                then 1
                else 0
        in
        fold_lefti (fun acc i _ ->
            acc + fold_lefti (fun acc j _ ->
                acc + attacking i j) 0 rows)
            0 rows
    );
    to_string = (function rows ->
        let add_row acc col =
            acc ^ match col with
            | 0 -> "Q . . . . . . .\n"
            | 1 -> ". Q . . . . . .\n"
            | 2 -> ". . Q . . . . .\n"
            | 3 -> ". . . Q . . . .\n"
            | 4 -> ". . . . Q . . .\n"
            | 5 -> ". . . . . Q . .\n"
            | 6 -> ". . . . . . Q .\n"
            | _ -> ". . . . . . . Q\n"
        in
        Array.fold_left add_row "" rows
    )
}
;;


let open Format in
let (queens, restarts) = simulated_annealing_search 64. eight_queens in
print_endline
@@ eight_queens.to_string
@@ queens;
printf "restarts: %d\n" restarts;

let avg_restarts n =
    let rec sum acc = function
        | 0 -> (Float.of_int acc) /. (Float.of_int n)
        | n ->
                let (_, restarts) = simulated_annealing_search 64. eight_queens in
                sum (acc + restarts) (n - 1)
    in
    sum 0 n
in
printf "avg. restarts: %f\n" (avg_restarts 10_000)

