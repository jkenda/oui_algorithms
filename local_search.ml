exception Unreachable;;

type dist = int;;
type 'a problem = {
    beam_size: int;
    (** generate a random state from a random int *)
    orig_f: int -> 'a;
    (** generate a list of next states for state *)
    next_f: 'a -> 'a list;
    (** how close the state is to the goal *)
    val_f: 'a -> dist;
    (** representation of the state as string *)
    to_string: 'a -> string
};;

Random.self_init ();;

(* beam local search *)
let beam_search { beam_size; orig_f; next_f; val_f; to_string } =
    let add_val state = (val_f state, state) in
    let make_beam rand_list =
        List.map (fun rand -> add_val (orig_f rand)) rand_list
    (* generate *beam size* random origins *)
    and gen_random_list beam_size =
        let rec gen' acc = function
        | 0 -> acc
        | n -> gen' (Random.full_int (Int.max_int) :: acc) (n - 1)
        in
        gen' [] beam_size
    (* generate states for next step *)
    and gen_next states =
        let gen_next' next (value, state) =
            List.map add_val (next_f state) @ next
        in
        List.fold_left gen_next' [] states
    (* put the best states into a beam *)
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
    let rec search' beam =
        let next_states = gen_next beam in
        let next_beam = filter_best next_states
        and (best_val, best) = List.hd beam in

        match List.nth_opt next_beam 0 with
        | Some (best_next_val, best_next) when best_next_val < best_val ->
                search' next_beam
        | _ -> best
    in
    gen_random_list beam_size
    |> make_beam
    |> search'
;;

let problem = {
    beam_size = 2;
    orig_f = (function rand ->
        match rand mod 4 with
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
assert (beam_search problem = [|0; 0; 0; 0|]);;
