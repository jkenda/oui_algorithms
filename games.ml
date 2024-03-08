open Tools

type ('a, 'b) game = {
    (** the current state *)
    state: 'a;
    (** generate a list of next states for state *)
    next_f: 'a -> 'a list;
    (** how many points?
        positive if we are winning, negative if we are losing *)
    val_f: 'a -> int;
    (** is the game over? true/false *)
    game_over: 'a -> int option;
    (** representation of the state as string *)
    to_string: 'a -> string
}
;;

Random.self_init ()

let shuffle d =
    let nd = List.map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort compare nd in
    List.map snd sond

let minimax start_depth { state; next_f; val_f; game_over; to_string } =
    let rec minimax' state depth is_max =
        match game_over state with
        | Some score -> score
        | None when depth = 0 -> val_f state
        | None ->
            let mM child = minimax' child (depth - 1) (not is_max) in
            let find curr_best child =
                let f = if is_max then max else min in
                f curr_best (mM child)
            in
            match shuffle (next_f state) with
            | hd :: tl -> List.fold_left find (mM hd) tl
            | [] -> val_f state
    in
    let min child = minimax' child start_depth false in
    let max_next ((max_v, _) as max) child =
        let v = min child in
        if v > max_v then (v, child) else max
    in
    match shuffle (next_f state) with
    | hd :: tl -> List.fold_left max_next (min hd, hd) tl
    | [] -> val_f state, state

