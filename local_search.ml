open Tools

exception Unreachable

type dist = int
type 'a problem = {
    (** generate a random state from a random int *)
    orig_f: (unit -> int) -> 'a;
    (** generate a list of next states for state *)
    next_f: 'a -> 'a list;
    (** how close the state is to the goal
        (0 means goal reached) *)
    val_f: 'a -> dist;
    (** representation of the state as string *)
    to_string: 'a -> string
}

let hill_climbing_search { orig_f; next_f; val_f; to_string } =
    let add_val state =
        (val_f state, state)
    in
    (* generate *beam size* random origins *)
    let init_state () =
        let gen_random_int () =
            Random.full_int (Int.max_int)
        in
        gen_random_int
        |> orig_f
        |> add_val
    in
    let rec search' (state_val, state) =
        if state_val = 0 then state
        else
            (* generate state's neighbours *)
            let best_next state =
                next_f state
                |> List.map add_val
                |> List.sort (fun (v1, _) (v2, _) -> v1 - v2)
                |> (function [] -> None | hd :: _ -> Some hd)
            in

            match best_next state with
            (* next state is better than the current - continue search *)
            | Some ((next_val, _) as next) when next_val < state_val ->
                    search' next
            (* goal not found -> restart *)
            | _ -> search' (init_state ())
    in
    init_state ()
    |> search'
;;

Random.self_init ();;

(* beam local search *)
let beam_search beam_size { orig_f; next_f; val_f; to_string } =
    let add_val state =
        (val_f state, state)
    in
    (* generate *beam size* random origins *)
    let init_beam beam_size =
        let gen_random_int () =
            Random.full_int (Int.max_int)
        in
        let gen_random_list beam_size =
            let rec gen' acc = function
            | 0 -> acc
            | n -> gen' (orig_f gen_random_int :: acc) (n - 1)
            in
            gen' [] beam_size
        in
        gen_random_list beam_size
        |> List.map add_val
    in
    (* execute beam search *)
    let rec search' visited beam =
        (* generate neighbourhood to the beam *)
        let gen_neigh beam =
            let gen_neigh' neigh (_, state) =
                let filter_visited state =
                    let val_state = add_val state in
                    if List.mem val_state visited then None
                    else Some val_state
                in
                List.filter_map filter_visited (next_f state) @ neigh
            in
            List.fold_left gen_neigh' [] beam
        (* filter the neighbourhood into a new beam *)
        and filter_best states =
            let rec trim len = function
                | [] -> []
                | _ :: _ when len = 0 -> []
                | hd :: tl -> hd :: trim (len - 1) tl
            in
            states
            |> List.sort (fun (v1, _) (v2, _) -> v1 - v2)
            |> trim beam_size
        in

        let (best_val, best) = List.hd beam in
        if best_val = 0 then best
        else
            let neighbours = gen_neigh beam in
            let next_beam = filter_best neighbours in
            match List.nth_opt next_beam 0 with
            (* next state is better than the current - continue search *)
            | Some (best_next_val, best_next) when best_next_val < best_val ->
                    search' (neighbours @ visited) next_beam
            (* goal not found -> restart *)
            | _ -> search' [] (init_beam beam_size)
    in
    init_beam beam_size
    |> search' []
;;

