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

(* beam local search search *)
let find_beam { beam_size; orig_f; next_f; val_f; to_string } =
    let origins =
        (* generate *beam size* random origins *)
        let gen_origin beam_size orig_f =
            let rec gen_random_list acc = function
                | 0 -> acc
                | n -> gen_random_list (Random.full_int (Int.max_int) :: acc) (n - 1)
            in
            gen_random_list [] beam_size
            |> List.map orig_f
        in
        gen_origin beam_size orig_f
    in
    (* generate states for next step *)
    let gen_next states =
        let gen_next' next state =
            next_f state @ next
        in
        List.fold_left gen_next' [] states
    (* put the mest states into a beam *)
    and filter_best states =
        let rec trim len = function
            | [] -> []
            | _ :: _ when len = 0 -> []
            | hd :: tl -> hd :: trim (len - 1) tl
        in
        states
        |> List.sort (fun a b -> val_f a - val_f b)
        |> trim beam_size
    and string_of_list f list =
        "[" ^ List.fold_left (fun acc x -> acc ^ " " ^ f x) "" list ^ " ]"
    in
    let rec aux states =
        print_endline (string_of_list to_string states);
        let neighbours = gen_next states in
        let next_states = filter_best neighbours in
        let best = List.hd states in

        match List.nth_opt next_states 0 with
        | Some best_next when val_f best_next < val_f best -> aux next_states
        | _ -> best
    in
    aux origins
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

assert (find_beam problem = [|0; 0; 0; 0|]);;
