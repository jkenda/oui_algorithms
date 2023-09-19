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
let (queens, restarts) = beam_search 8 eight_queens in
print_endline
@@ eight_queens.to_string
@@ queens;

let run n algo =
    let rec sum acc = function
        | 0 -> (Float.of_int acc) /. (Float.of_int n)
        | n ->
                let (_, restarts) = algo eight_queens in
                sum (acc + restarts) (n - 1)
    in
    let start = Unix.gettimeofday () in
    let restarts = sum 0 n in
    (Unix.gettimeofday () -. start, restarts)
in

let iter = 10_000 in

print_endline "+-----------+----------------+---------+----------+";
print_endline "| algorithm | params         | speedup | restarts |";

let (time_hill, restarts) = run iter (hill_climbing_search) in
printf        "| hill cl.  |              / | %7.3f | %8.3f |\n" 1. restarts; flush stdout;

let (time, restarts) = run iter (beam_search 1) in
printf        "| beam se.  | beam_size = 1  | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (beam_search 2) in
printf        "| beam se.  | beam_size = 2  | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (beam_search 4) in
printf        "| beam se.  | beam_size = 4  | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (beam_search 8) in
printf        "| beam se.  | beam_size = 8  | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (beam_search 16) in
printf        "| beam se.  | beam_size = 16 | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (beam_search 32) in
printf        "| beam se.  | beam_size = 32 | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;

let (time, restarts) = run iter (simulated_annealing_search 1.) in
printf        "| sim. an.  | init_temp = 1  | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (simulated_annealing_search 2.) in
printf        "| sim. an.  | init_temp = 2  | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (simulated_annealing_search 4.) in
printf        "| sim. an.  | init_temp = 4  | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (simulated_annealing_search 8.) in
printf        "| sim. an.  | init_temp = 8  | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (simulated_annealing_search 16.) in
printf        "| sim. an.  | init_temp = 16 | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (simulated_annealing_search 32.) in
printf        "| sim. an.  | init_temp = 32 | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;
let (time, restarts) = run iter (simulated_annealing_search 64.) in
printf        "| sim. an.  | init_temp = 64 | %7.3f | %8.3f |\n" (time_hill /. time) restarts; flush stdout;

print_endline "+-----------+----------------+---------+----------+";
