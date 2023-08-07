open Graph
open Tools

let sliding_puzzle_3x3 = {
    (* 0 means empty *)
    origin =
        [|7; 2; 4;
          5; 0; 6;
          8; 3; 1|];
    goals = [
        [|1; 2; 3;
          4; 5; 6;
          7; 8; 0|]
    ];

    next_f = (function state ->
        let find_empty state =
            let find_empty' prev i tile =
                if tile = 0 then (i / 3, i mod 3) else prev
            in
            fold_lefti find_empty' (0, 0) state
        and swap (i1, j1) (i2, j2) =
            let idx1 = (3 * i1 + j1)
            and idx2 = (3 * i2 + j2)
            and copy = Array.copy state in
            copy.(idx1) <- state.(idx2);
            copy.(idx2) <- state.(idx1);
            copy, 1
        and neighbours = function
            | (0, 0) -> [(0, 1); (1, 0)]
            | (0, 1) -> [(0, 0); (0, 2); (1, 1)]
            | (0, 2) -> [(0, 1); (1, 2)]
            | (1, 0) -> [(1, 1); (0, 0); (2, 0)]
            | (1, 1) -> [(1, 0); (1, 2); (0, 1); (2, 1)]
            | (1, 2) -> [(1, 1); (0, 2); (2, 2)]
            | (2, 0) -> [(2, 1); (1, 0)]
            | (2, 1) -> [(2, 0); (2, 2); (1, 1)]
            | (_, _) -> [(2, 1); (1, 2)]
        in
        let empty = find_empty state in
        List.map (swap empty) (neighbours empty)
    );

    heur_f = (function state ->
        let desired_pos = function
            | 1 -> (0, 0)
            | 2 -> (0, 1)
            | 3 -> (0, 2)
            | 4 -> (1, 0)
            | 5 -> (1, 1)
            | 6 -> (1, 2)
            | 7 -> (2, 0)
            | 8 -> (2, 1)
            | _ -> (2, 2)
        and current_pos i =
            (i / 3, i mod 3)
        and manhattan_dist (a, b) (x, y) =
            Int.abs (a - x) + Int.abs (b - y)
        in
        let add_dist sum i tile =
            sum + manhattan_dist (current_pos i) (desired_pos tile)
        in
        fold_lefti add_dist 0 state
    );
    to_string = (function state ->
        let string_of_tile i tile =
            let tile_of_int = function
                | 0 -> ' '
                | n -> Char.chr (n + Char.code '0')
            in
            Char.escaped (tile_of_int tile);
            ^ (if i mod 3 = 2 then "\n" else " ")
        in
        fold_lefti (fun acc i x -> acc ^ string_of_tile i x) "" state
    )
};;


match find_a' sliding_puzzle_3x3 with
| Some (path, steps) ->
        print_string @@ string_of_path sliding_puzzle_3x3.to_string path;
        Format.printf "steps: %d\n" steps
| None -> print_endline "not found"

