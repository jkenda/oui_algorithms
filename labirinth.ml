open Local_search

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

print_endline
@@ labirinth.to_string
@@ beam_search 2 labirinth

