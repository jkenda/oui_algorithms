open Tools

type 'a game = {
    (** the current state *)
    state: 'a;
    (** generate a list of next states for state *)
    next_f: 'a -> 'a list;
    (** how many points?
        positive if we are winning, negative if we are losing *)
    val_f: 'a -> int;
    (** is the game over? true/false *)
    game_over: 'a game -> bool;
    (** representation of the state as string *)
    to_string: 'a game -> string
}
;;

let minimax start_depth ({ state; next_f; val_f; game_over; to_string } as game) =
    let rec minimax' state depth max =
        let choose children =
            let eval curr_best child =
                let f = if max then Int.max else Int.min in
                f curr_best (minimax' child (depth - 1) (not max))
            in
            match children with
            | hd :: tl -> List.fold_left eval (val_f hd) tl
            | [] -> val_f state
        in

        if depth = 0 || game_over game then
            val_f state
        else
            choose (next_f state)
    in
    let max_next ((max_v, _) as max) child =
        let v = minimax' child start_depth true in
        if v > max_v then (v, child) else max
    in
    match next_f state with
    | hd :: tl -> List.fold_left max_next (val_f hd, hd) tl
    | [] -> val_f state, state

