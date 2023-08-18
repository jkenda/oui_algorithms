open Tools

type dist = int
type 'a problem = {
    (** generate a random state from a random int *)
    orig_f: (unit -> int) -> 'a;
    (** generate a list of next states for state *)
    next_f: 'a -> 'a list;
    (** how close the state is to the goal
        (0 means goal reached) *)
    dist_f: 'a -> dist;
    (** representation of the state as string *)
    to_string: 'a -> string
}
;;

Random.self_init ()

(* get random int *)
let gen_random_int () =
    Random.full_int (Int.max_int)
;;
(* add distance to state *)
let add_dist dist_f state =
    (dist_f state, state)
;;
(* generate a random origin *)
let init_state orig_f dist_f =
    add_dist dist_f @@ orig_f gen_random_int
;;
(* generate state's neighbours *)
let best_next states =
    let min' ((min_v, _) as min) ((v, _) as state) =
        if v < min_v then state else min
    in
    match states with
    | [] -> None
    | hd :: tl -> Some (List.fold_left min' hd tl)
;;

let hill_climbing_search { orig_f; next_f; dist_f; to_string } =
    (* generate neighbouring states;
     filter out those that are:
         - worse than the current
         - already visited *)
    let gen_neigh (dist, state) visited =
        let keep_relevant state =
            let (d, _) as ds = (dist_f state, state) in
            if d >= dist || List.mem ds visited
            then None else Some ds
        in
        List.filter_map keep_relevant (next_f state)
    in
    let rec search' restarts visited ((dist, state) as ds) =
        if dist = 0 then state, restarts
        else
            let neigh = gen_neigh ds visited in
            match best_next neigh with
            (* next state is better than the current - continue search *)
            | Some ((next_dist, _) as next) when next_dist < dist ->
                    search' restarts (neigh @ visited) next
            (* goal not found -> restart *)
            | _ -> search' (restarts + 1) [] @@ init_state orig_f dist_f
    in
    init_state orig_f dist_f
    |> search' 0 []
;;

let simulated_annealing_search init_temp { orig_f; next_f; dist_f; to_string } =
    (* generate neighbouring states *)
    let gen_neigh state =
        next_f state
        |> List.map (add_dist dist_f)
    in
    let rec search' restarts temp ((curr_dist, curr_state) as curr) =
        (* dist. to solution goal 0 - goal found *)
        if curr_dist = 0 then curr_state, restarts
        else
            (* solution not found - restart *)
            if temp = 0. then search' (restarts + 1) init_temp @@ init_state orig_f dist_f
            else
                let neigh = gen_neigh curr_state in
                match best_next neigh with
                | None -> search' (restarts + 1) init_temp @@ init_state orig_f dist_f
                | Some ((best_dist, _) as best) -> 
                    search' restarts (temp /. 2.) @@
                        (* if there is a better neighbour, use it *)
                        if best_dist < curr_dist then best
                        else
                            (* otherwise use either a random worse state or the current one *)
                            let (next_dist, _) as next =
                                List.nth neigh
                                @@ Random.full_int
                                @@ List.length neigh
                            in
                            let delta = Int.to_float (next_dist - curr_dist) in
                            if Random.float 1. < (Float.exp (delta /. temp)) then next else curr
    in
    search' 0 init_temp @@ init_state orig_f dist_f
;;

(* beam local search *)
let beam_search beam_size { orig_f; next_f; dist_f; to_string } =
    (* generate *beam size* random origins *)
    let init_beam beam_size =
        let gen_random_list beam_size =
            let rec gen' acc = function
            | 0 -> acc
            | n -> gen' (orig_f gen_random_int :: acc) (n - 1)
            in
            gen' [] beam_size
        in
        gen_random_list beam_size
        |> List.map (add_dist dist_f)
    in
    (* execute beam search *)
    let rec search' restarts visited beam =
        (* generate neighbourhood to the beam *)
        let gen_neigh beam =
            let gen_neigh' neigh (_, state) =
                let filter_visited state =
                    let (state_dist, _) = List.hd beam
                    and (v, _) as state = (add_dist dist_f) state in
                    if v >= state_dist || List.mem state visited
                    then None else Some state
                in
                List.filter_map filter_visited (next_f state) @ neigh
            in
            List.fold_left gen_neigh' [] beam
        (* filter the neighbourhood into a new beam *)
        and filter_best states =
            let rec trim len acc = function
                | [] -> acc
                | _ :: _ when len = 0 -> acc
                | hd :: tl -> trim (len - 1) (hd :: acc) tl
            in
            states
            |> List.sort (fun (v1, _) (v2, _) -> v1 - v2)
            |> trim beam_size []
            |> List.rev
        in

        let (best_dist, best) = List.hd beam in
        if best_dist = 0 then best, restarts
        else
            let neighbours = gen_neigh beam in
            let next_beam = filter_best neighbours in
            match List.nth_opt next_beam 0 with
            (* next state is better than the current - continue search *)
            | Some (best_next_dist, best_next) when best_next_dist <= best_dist ->
                    search' restarts (neighbours @ visited) next_beam
            (* goal not found -> restart *)
            | _ -> search' (restarts + 1) [] (init_beam beam_size)
    in
    init_beam beam_size
    |> search' 0 []
;;

