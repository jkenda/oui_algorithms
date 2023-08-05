exception Unreachable;;

type dist = int;;
type 'a problem = {
    origins: 'a array;
    next_f: 'a -> 'a list;
    heur_f: 'a -> dist
};;

let problem = {
    origins = [|
        [|1; 0; 0; 0|];
        [|0; 1; 0; 0|]
    |];
    next_f = (function
        | [|1; 0; 0; 0|] -> [ [|0; 1; 0; 0|] ]
        | [|0; 1; 0; 0|] -> [ [|1; 0; 0; 0|]; [|0; 0; 1; 0|] ]
        | [|0; 0; 1; 0|] -> [ [|0; 1; 0; 0|]; [|0; 0; 0; 1|] ]
        | [|0; 0; 0; 1|] -> [ [|0; 0; 1; 0|]; [|0; 0; 0; 0|] ]
        | [|0; 0; 0; 0|] -> [ [|0; 0; 0; 1|] ]
        | _ -> raise Unreachable
    );
    heur_f = (function
        | [|1; 0; 0; 0|] -> 4
        | [|0; 1; 0; 0|] -> 3
        | [|0; 0; 1; 0|] -> 2
        | [|0; 0; 0; 1|] -> 1
        | [|0; 0; 0; 0|] -> 0
        | _ -> raise Unreachable
    )
}

let fold_lefti f x a =
    let open Array in
    let r = ref x in
    for i = 0 to length a - 1 do
        r := f !r i (unsafe_get a i)
    done;
    !r
;;

(* beam local search search *)
let find_beam { origins; next_f; heur_f } =
    let gen_next visited states =
        let gen_next' next state =
            if List.mem state visited then next
            else state :: next
        in
        Array.fold_left gen_next' [] states
    and filter_best len states =
        let rec trim len list =
            match len, list with
            | _, [] -> []
            | 0, hd :: tl -> []
            | n, hd :: tl ->
                    hd :: trim (n - 1) tl
        in
        states
        |> List.sort (fun a b -> heur_f a - heur_f b)
        |> trim len
        |> Array.of_list
    in
    let best_state = function
        | [||] -> None
        | states -> Some states.(0)
    in
    let rec aux visited states =
            let neighbours = gen_next visited states in
            let next_states = filter_best (Array.length states) neighbours in
            let best = Option.get (best_state states) in
            match best_state next_states with
            | Some best_next when heur_f best_next < heur_f best -> aux (neighbours @ visited) next_states
            | _ -> best
    in
    aux [] origins
;;
assert (find_beam problem = [|0; 0; 0; 0|]);;
