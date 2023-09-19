open Tools
open Games

type tile = X | O | E
let string_of_tile = function
    | X -> "X"
    | O -> "O"
    | E -> "."
let other_player = function
    | X -> O
    | O -> X
    | _ -> raise Unreachable

let count state tile =
    let count' acc tile' =
        acc + if tile' = tile then 1 else 0
    in Array.fold_left count' 0 state

let human = O
let starting_player = other_player human

let rec tic_tac_toe = {
    state =
        (starting_player,
        [|E; E; E;
          E; E; E;
          E; E; E|]
        );
    next_f = (function (player, board) ->
        let neigh states i tile =
            let add_o i =
                let copy = Array.copy board in
                copy.(i) <- player;
                other_player player, copy
            in
            if tile = E then add_o i :: states
            else states
        in
        fold_lefti neigh [] board
    );
    val_f = (function (player, board) ->
        let count = count board in
        let matches a b c =
            if a != E && a = b && a = c then
                (if a = player then 1 else -1)
            else 0
        in
        match board with
        | [|a; b; c;
            d; e; f;
            g; h; i|] ->
                let finished =
                    matches a b c +
                    matches d e f +
                    matches g h i +
                    matches a d g +
                    matches b e h +
                    matches c f i +
                    matches a e i +
                    matches g e c
                in
                finished * (1 + count E)
        | _ -> 0
    );
    game_over = (function ((player, board) as state) ->
        let score = tic_tac_toe.val_f state in
        if score = 0 && count board E > 0 then None
        else Some score
    );
    to_string = (function (player, board) ->
        let string_of_state string i tile =
            string
            ^ (if i mod 3 = 0 then Char.escaped (Char.chr (i / 3 + Char.code 'a')) else "") ^ " "
            ^ (string_of_tile tile)
            ^ (if i mod 3 = 2 then "\n" else "")
        in
        fold_lefti string_of_state "  1 2 3\n" board
    )
} 

(** play the game, everybody play the game *)
let rec play game depth =
    let (player, board) = game.state in
    let rec ask () =
        let index_of_location c n =
            3 * (Char.compare c 'a') + (n - 1)
        in
        print_string "> "; flush stdout;
        let i = try Scanf.scanf "%c%d\n" index_of_location with _ -> ask () in
        if 0 <= i && i < 9 && board.(i) = E then i
        else ask ()
    in
    let game, fail, mM_score =
        if player = human then (
            (* print the board *)
            print_string (tic_tac_toe.to_string game.state);
            (* ask where to place the sign *)
            board.(ask ()) <- player;
            { game with state = (other_player player, board) }, false, 1)
        else (
            let mM_score, next_state = minimax depth game in
            { game with state = next_state }, false, mM_score)
    in

    if mM_score = 0 then (
        print_string (tic_tac_toe.to_string game.state);
        None)
    else
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

