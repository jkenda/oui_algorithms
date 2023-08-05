open Graph

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
        let empty_coord =
            let find_empty prev i tile =
                if tile = 0 then (i / 3, i mod 3) else prev
            in
            fold_lefti find_empty (0, 0) state
        in
        let swap (i2, j2) =
            let (i1, j1) = empty_coord in
            let idx1 = (3 * i1 + j1)
            and idx2 = (3 * i2 + j2)
            and copy = Array.copy state in
            copy.(idx1) <- copy.(idx2);
            copy.(idx2) <- 0;
            copy, 1
        in
        match empty_coord with
        | (0, 0) -> [swap (0, 1); swap (1, 0)]
        | (0, 1) -> [swap (0, 0); swap (0, 2); swap (1, 1)]
        | (0, 2) -> [swap (0, 1); swap (2, 1)]
        | (1, 0) -> [swap (1, 1); swap (0, 0); swap (2, 0)]
        | (1, 1) -> [swap (1, 0); swap (1, 2); swap (0, 1); swap (2, 1)]
        | (1, 2) -> [swap (1, 1); swap (0, 2); swap (2, 2)]
        | (2, 0) -> [swap (2, 1); swap (1, 0)]
        | (2, 1) -> [swap (2, 0); swap (2, 2); swap (1, 1)]
        | (2, 2) -> [swap (2, 1); swap (1, 2)]
        | (_, _) -> raise Unreachable
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
        and _euclid_dist (a, b) (x, y) =
            let d1 = a - x and d2 = b - y in
            (d1 * d1 + d2 * d2) |> Int.to_float |> Float.sqrt |> Float.to_int
        in
        let add_dist sum i tile =
            sum + manhattan_dist (current_pos i) (desired_pos tile)
        in
        fold_lefti add_dist 0 state
    )
};;

let print_path path =
    let print_state state =
        let print_tile i tile =
            let tile_of_int = function
                | 0 -> ' '
                | n -> Char.chr (n + Char.code '0')
            in
            print_char (tile_of_int tile);
            print_char (if i mod 3 = 2 then '\n' else ' ')
        in
        fold_lefti (fun _ i x -> print_tile i x) () state;
        print_char '\n'
    in
    let rec aux = function
        | [] -> ()
        | hd :: tl -> print_state hd; aux tl
    in aux path
;;


match find_a' sliding_puzzle_3x3 with
| Some (path, steps) -> print_path path; Format.printf "steps: %d\n" steps
| None -> print_endline "not found"

