open Tools
open Games

type tile = X | O | E;;
let string_of_tile = function
    | X -> "X"
    | O -> "O"
    | E -> " "
;;

let count state =
    let count' acc tile =
        acc + if tile != E then 1 else 0
    in Array.fold_left count' 0 state
;;

let rec tic_tac_toe = {
    state =
        [|E; E; E;
          E; E; E;
          E; E; E|];
    next_f = (function state ->
        let neigh states i tile =
            let add_o i =
                let copy = Array.copy state in
                copy.(i) <- O;
                copy
            in
            if tile = E then add_o i :: states
            else states
        in
        fold_lefti neigh [] state
    );
    val_f = (function state ->
        match state with
        | [|O; O; O;
            _; _; _;
            _; _; _|]
        | [|_; _; _;
            O; O; O;
            _; _; _|]
        | [|_; _; _;
            _; _; _;
            O; O; O|]
        | [|O; _; _;
            O; _; _;
            O; _; _|]
        | [|_; O; _;
            _; O; _;
            _; O; _|]
        | [|_; _; O;
            _; _; O;
            _; _; O|]
        | [|O; _; _;
            _; O; _;
            _; _; O|]
        | [|_; _; O;
            _; O; _;
            O; _; _|] -> 9 - count state
        | [|X; X; X;
            _; _; _;
            _; _; _|]
        | [|_; _; _;
            X; X; X;
            _; _; _|]
        | [|_; _; _;
            _; _; _;
            X; X; X|]
        | [|X; _; _;
            X; _; _;
            X; _; _|]
        | [|_; X; _;
            _; X; _;
            _; X; _|]
        | [|_; _; X;
            _; _; X;
            _; _; X|]
        | [|X; _; _;
            _; X; _;
            _; _; X|]
        | [|_; _; X;
            _; X; _;
            X; _; _|] -> -(9 - count state)
        | _ -> 0
    );
    game_over = (function game ->
        tic_tac_toe.val_f game.state != 0 || count game.state = 9
    );
    to_string = (function game ->
        let string_of_state string i tile =
            string
            ^ (if i mod 3 = 0 then string_of_int (i / 3 + 1) else "") ^ " "
            ^ (string_of_tile tile)
            ^ (if i mod 3 = 2 then "\n" else "")
        in
        fold_lefti string_of_state "  1 2 3\n" game.state
    )
} 
;;
    

let () =
    let rec play game player depth =
        let game, fail =
            if player then
                (print_string (tic_tac_toe.to_string game);
                print_string "> "; flush stdout;
                let i = Scanf.scanf "%d, %d\n" (fun i j -> 3 * (i-1) + (j-1)) in
                if 0 <= i && i < 9 && game.state.(i) = E then
                    (game.state.(i) <- X;
                    game, true)
                else game, false)
            else
                (let _, next_state = minimax depth game in
                { game with state = next_state }, true)
        in

        if tic_tac_toe.game_over game then
            (print_string (tic_tac_toe.to_string game);
            tic_tac_toe.val_f game.state)
        else
            let player = (if fail then not else Fun.id) player in
            let depth = depth - if fail then 0 else 1 in
            play game player depth
    in

    let score = play tic_tac_toe false 9 in
    print_endline ("game over, "
        ^ (if score = 0 then "draw!"
            else if score < 0 then "you won!"
            else "you lost!"))
