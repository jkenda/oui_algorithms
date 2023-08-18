open Tools
open Games

type tile = X | O | E
let string_of_tile = function
    | X -> "X"
    | O -> "O"
    | E -> " "
let next_player = function
    | X -> O
    | O -> X
    | _ -> raise Unreachable

let count state =
    let count' acc tile =
        acc + if tile != E then 1 else 0
    in Array.fold_left count' 0 state

let rec tic_tac_toe = {
    state =
        (X,
        [|E; E; E;
          E; E; E;
          E; E; E|]
        );
    next_f = (function (player, board) ->
        let neigh states i tile =
            let add_o i =
                let copy = Array.copy board in
                copy.(i) <- player;
                next_player player, copy
            in
            if tile = E then add_o i :: states
            else states
        in
        fold_lefti neigh [] board
    );
    val_f = (function (player, board) ->
        let matches a b c =
            if a != E && a = b && a = c then
                (if a = player then 1 else -1)
            else 0
        in
        match board with
        | [|a; b; c;
            d; e; f;
            g; h; i|] ->
                (matches a b c +
                matches d e f +
                matches g h i +
                matches a d g +
                matches b e h +
                matches c f i +
                matches a e i +
                matches g e c)
                * (9 - count board)
        | _ -> 0
    );
    game_over = (function ((player, board) as state) ->
        let score = tic_tac_toe.val_f state in
        if score = 0 && count board < 9 then None
        else Some score
    );
    to_string = (function (player, board) ->
        let string_of_state string i tile =
            string
            ^ (if i mod 3 = 0 then string_of_int (i / 3 + 1) else "") ^ " "
            ^ (string_of_tile tile)
            ^ (if i mod 3 = 2 then "\n" else "")
        in
        fold_lefti string_of_state "  1 2 3\n" board
    )
} 

let human = O

let rec play game depth =
    let (player, board) = game.state in
    let game, fail =
        if player = human then
            (print_string (tic_tac_toe.to_string game.state);
            print_string "> "; flush stdout;
            let i = Scanf.scanf "%d, %d\n" (fun i j -> 3 * (i-1) + (j-1)) in
            if 0 <= i && i < 9 && board.(i) = E then
                (board.(i) <- player;
                { game with state = (next_player player, board) }, false)
            else game, true)
        else
            (let _, next_state = minimax depth game in
            { game with state = next_state }, false)
    in

    match tic_tac_toe.game_over game.state with
    | None -> play game (depth - if fail then 0 else 1)
    | Some score ->
        print_string (tic_tac_toe.to_string game.state);
        if score = 0 then None
        else Some player

let () =
    let winner = play tic_tac_toe 9 in
    print_endline ("game over, " ^
    match winner with
    | None -> "draw!"
    | Some p ->
            if p = human then "you won!"
            else "you lost!")

