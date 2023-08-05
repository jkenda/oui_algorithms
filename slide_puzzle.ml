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
    next_f = (function
        | [|0; b; c;
            d; e; f;
            g; h; i|] -> [
                [|b; 0; c;
                  d; e; f;
                  g; h; i|], 1;
                [|d; b; c;
                  0; e; f;
                  g; h; i|], 1
            ]
        | [|a; 0; c;
            d; e; f;
            g; h; i|] -> [
                [|a; e; c;
                  d; 0; f;
                  g; h; i|], 1;
                [|a; c; 0;
                  d; e; f;
                  g; h; i|], 1;
                [|0; a; c;
                  d; e; f;
                  g; h; i|], 1
            ];
        | [|a; b; 0;
            d; e; f;
            g; h; i|] -> [
                [|a; 0; b;
                  d; e; f;
                  g; h; i|], 1;
                [|a; b; f;
                  d; e; 0;
                  g; h; i|], 1
            ];
        | [|a; b; c;
            0; e; f;
            g; h; i|] -> [
                [|0; b; c;
                  a; e; f;
                  g; h; i|], 1;
                [|a; b; c;
                  e; 0; f;
                  g; h; i|], 1;
                [|a; b; c;
                  g; e; f;
                  0; h; i|], 1
            ];
        | [|a; b; c;
            d; 0; f;
            g; h; i|] -> [
                [|a; 0; c;
                  d; b; f;
                  g; h; i|], 1;
                [|a; b; c;
                  0; d; f;
                  g; h; i|], 1;
                [|a; b; c;
                  d; f; 0;
                  g; h; i|], 1;
                [|a; b; c;
                  d; h; f;
                  g; 0; i|], 1
            ];
        | [|a; b; c;
            d; e; 0;
            g; h; i|] -> [
                [|a; b; 0;
                  d; e; c;
                  g; h; i|], 1;
                [|a; b; c;
                  d; 0; e;
                  g; h; i|], 1;
                [|a; b; c;
                  d; e; i;
                  g; h; 0|], 1
            ];
        | [|a; b; c;
            d; e; f;
            0; h; i|] -> [
                [|a; b; c;
                  0; e; f;
                  d; h; i|], 1;
                [|a; b; c;
                  d; e; f;
                  h; 0; i|], 1
            ];
        | [|a; b; c;
            d; e; f;
            g; 0; i|] -> [
                [|a; b; c;
                  d; 0; f;
                  g; e; i|], 1;
                [|a; b; c;
                  d; e; f;
                  0; g; i|], 1;
                [|a; b; c;
                  d; e; f;
                  g; i; 0|], 1;
            ];
        | [|a; b; c;
            d; e; f;
            g; h; 0|] -> [
                [|a; b; c;
                  d; e; 0;
                  g; h; f|], 1;
                [|a; b; c;
                  d; e; f;
                  g; 0; h|], 1
            ];
        | _ -> raise Unreachable);
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
        in
        let current_pos i =
            (i / 3, i mod 3)
        in
        let manhattan_dist (a, b) (x, y) =
            Int.abs (a - x) + Int.abs (b - y)
        in
        let _euclid_dist (a, b) (x, y) =
            let d1 = a - x and d2 = b - y in
            (d1 * d1 + d2 * d2) |> Int.to_float |> Float.sqrt |> Float.to_int
        in
        fold_lefti (fun sum i x -> sum + manhattan_dist (current_pos i) (desired_pos x)) 0 state)
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

