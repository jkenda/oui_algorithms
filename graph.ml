open Tools

type dist = int;;
type 'a graph = {
    origin: 'a;
    goals: 'a list;
    next_f: 'a -> ('a * dist) list;
    heur_f: 'a -> dist
};;

let graph1 = {
    origin = 'a'; goals = ['h'];
    next_f = (function 'a' -> ['b', 1; 'c', 2] | 'b' | 'c' -> [] | _ -> raise Unreachable);
    heur_f = (function _ -> 0)
};;

let graph2 = {
    origin = 'a'; goals = ['c'];
    next_f = (function 'a' -> ['b', 1; 'c', 2] | 'b' | 'c' -> [] | _ -> raise Unreachable);
    heur_f = (function _ -> 0)
};;

let graph3 = {
    origin = 'a'; goals = ['h'];
    next_f = (function 'a' -> ['b', 1; 'c', 2] | 'b' -> [] | 'c' -> ['h', 3] | _ -> raise Unreachable);
    heur_f = (function _ -> 0)
}

(* depth first search *)
let find_dfs { origin; goals; next_f; _ } =
    let rec search_node path sum self =
        let rec search_list = function
            | [] -> None
            | (child, dist) :: tl ->
                    match search_node (self :: path) (sum + dist) child with
                    | None -> search_list tl
                    | r -> r
        in
        (* goal reached *)
        if List.mem self goals then Some (self :: path, sum)
        (* prevent cycling *)
        else if List.mem self path then None
        (* search children *)
        else search_list (next_f self)
    in
    search_node [] 0 origin
        |> Option.map (fun (path, distance) -> (List.rev path, distance))
;;
assert (find_dfs graph1 = None);;
assert (find_dfs graph2 = Some (['a'; 'c'], 2));;
assert (find_dfs graph3 = Some (['a'; 'c'; 'h'], 5));;

(* breadth first search *)
let find_bfs { origin; goals; next_f; _ } =
    let add_to_queue queue path sum children =
        let filter ((node, dist) as child) =
            if List.mem node path then None
            else Some (node :: path, sum + dist, child)
        in
        queue @ List.filter_map filter children
    in
    let rec aux = function
        (* queue is exhausted, none of the goals found *)
        | [] -> None
        (* there are still elements in the queue *)
        | (path, dist, (self, _)) :: queue ->
                (* goal found *)
                if List.mem self goals then Some (List.rev path, dist)
                else aux (add_to_queue queue path dist (next_f self))
    in
    aux [([origin], 0, (origin, 0))]
;;
assert (find_bfs graph1 = None);;
assert (find_bfs graph2 = Some (['a'; 'c'], 2));;
assert (find_bfs graph3 = Some (['a'; 'c'; 'h'], 5));;

type 'a container =
    | Bottom
    | Limit
    | Found of 'a
;;
let container_map f = function
    | Bottom | Limit as bl -> bl
    | Found x -> Found (f x)
;;

(* iterative deepeining depth first search *)
let find_ids { origin; goals; next_f; _ } =
    let find_dfs_limited limit =
        let rec search_node path sum self limit =
            let rec search_list = function
                | [] -> Bottom
                | (child, dist) :: tl ->
                        match search_node (self :: path) (sum + dist) child (limit - 1) with
                        | Limit ->
                                (match search_list tl with
                                 | Bottom | Limit -> Limit
                                 | r -> r)
                        | Bottom -> search_list tl
                        | r -> r
            in
            (* goal reached *)
            if List.mem self goals then Found (self :: path, sum)
            (* limit reached *)
            else if limit < 0 then Limit
            (* prevent cycling *)
            else if List.mem self path then Bottom
            (* search children *)
            else
                search_list (next_f self)
        in
        search_node [] 0 origin limit
            |> container_map (fun (path, distance) -> (List.rev path, distance))
    in
    let rec aux limit =
        match find_dfs_limited limit with
        | Bottom -> None
        | Limit -> aux (limit + 1)
        | Found r -> Some r
    in
    aux 0
;;
assert (find_ids graph1 = None);;
assert (find_ids graph2 = Some (['a'; 'c'], 2));;
assert (find_ids graph3 = Some (['a'; 'c'; 'h'], 5));;


let graph = {
    origin = 's'; goals = ['l'; 'o'];
    next_f = (function
        | 's' -> ['a', 3; 'b', 2; 'c', 2]
        | 'a' -> ['d', 3; 'e', 5]
        | 'b' -> ['e', 4; 'f', 3; 'g', 3]
        | 'c' -> ['g', 1; 'h', 5]
        | 'd' -> ['i', 3; 'j', 1]
        | 'e' -> ['j', 2; 'l', 1]
        | 'f' -> ['m', 3; 'n', 1]
        | 'g' -> []
        | 'h' -> ['o', 2; 'p', 1]
        | 'i' -> []
        | 'j' -> ['k', 1]
        | 'k' -> ['e', 1]
        | 'l' -> []
        | 'm' -> []
        | 'n' -> ['o', 2]
        | 'o' -> []
        | 'p' -> []
        | _ -> raise Unreachable);
    heur_f = (function _ ->
        raise (Failure "the default heur_f is a placeholder"))
};;

let correct_h = function 's' -> 4 | 'a' -> 5 | 'b' -> 5 | 'c' -> 5
                       | 'd' -> 1 | 'e' -> 1 | 'f' -> 3 | 'g' -> 6
                       | 'h' -> 2 | 'i' -> 1 | 'j' -> 2 | 'k' -> 1
                       | 'l' -> 0 | 'm' -> 6 | 'n' -> 2 | 'o' -> 0 | 'p' -> 1
                       | _ -> raise (Failure "invalid node reached");;
let perfect_h = function 's' -> 7 | 'a' -> 6 | 'b' -> 5 | 'c' -> 7
                       | 'd' -> 4 | 'e' -> 1 | 'f' -> 3 | 'g' -> Int.max_int
                       | 'h' -> 2 | 'i' -> Int.max_int  | 'j' -> 3
                       | 'k' -> 2 | 'l' -> 0 | 'm' -> Int.max_int
                       | 'n' -> 2 | 'o' -> 0 | 'p' -> Int.max_int
                       | _ -> raise (Failure "invalid node reached");;

let incorrect_h = function 's' -> 4 | 'a' -> 5 | 'b' -> 6 | 'c' -> 5
                         | 'd' -> 1 | 'e' -> 4 | 'f' -> 3 | 'g' -> 6
                         | 'h' -> 5 | 'i' -> 1 | 'j' -> 2 | 'k' -> 1
                         | 'l' -> 2 | 'm' -> 6 | 'n' -> 2 | 'o' -> 0 | 'p' -> 1
                         | _ -> raise (Failure "invalid node reached");;

assert (find_bfs graph = Some (['s'; 'a'; 'e'; 'l'], 9));;
assert (find_dfs graph = Some (['s'; 'a'; 'd'; 'j'; 'k'; 'e'; 'l'], 10));;
assert (find_ids graph = find_bfs graph);;


(* greedy best-first search *)
let find_greedy { origin; goals; next_f; heur_f } =
    let rec search_node path sum self =
        let search_min_child children =
            let rec min ((min, _, _) as prev) (hd, dist) =
                let h = heur_f hd in
                if h < min then (h, Some hd, dist) else prev
            in
            let (_, child, dist) = List.fold_left min (Int.max_int, None, 0) children in
            match child with
            | Some child when not (List.mem child path) -> search_node (self :: path) (sum + dist) child
            | _ -> None
        in
        (* goal reached *)
        if List.mem self goals then Some (self :: path, sum)
        (* prevent cycling *)
        else if List.mem self path then None
        (* search children *)
        else search_min_child (next_f self)
    in
    search_node [] 0 origin
    |> Option.map (fun (path, distance) -> (List.rev path, distance))
;;
assert (find_greedy { graph with heur_f = perfect_h } = Some (['s'; 'b'; 'e'; 'l'], 7));;
assert (find_greedy { graph with heur_f = correct_h } = None);;

(* A* *)
let find_a' { origin; goals; next_f; heur_f } =
    let open PrioQueue in
    (* add neighbours' properties to queue *)
    let add_to_queue queue path sum neighbours =
        let insert' queue ((self, dist) as node) =
            if List.mem self path then queue (* prevent cycling *)
            else
                let (dist, _, _) as node = sum + dist, node, self :: path in
                insert queue (dist + heur_f self) node
        in
        List.fold_left insert' queue neighbours
    in
    let rec aux = function
        (* queue is exhausted, none of the goals found *)
        | Empty -> None
        (* there are still elements in the queue *)
        | queue ->
                let (_, (dist, (self, _), path), queue) = extract queue in
                (* goal found *)
                if List.mem self goals then Some (List.rev path, dist)
                else aux (add_to_queue queue path dist (next_f self))
    in
    aux (leaf 0 (0, (origin, 0), [origin]))
;;
assert (find_a' { graph with heur_f = correct_h } = Some (['s'; 'b'; 'e'; 'l'], 7));;
assert (find_a' { graph with heur_f = perfect_h } = find_a' { graph with heur_f = correct_h });;

assert (find_a' { graph with heur_f = incorrect_h } = Some (['s'; 'b'; 'f'; 'n'; 'o'], 8));;
assert (find_a' { graph with heur_f = (fun _ -> 0); goals = [' ']   } = None);;

type ('a, 'b) result =
    | Result of 'a
    | Limit of 'b
;;

(* IDA* *)
let find_ida' { origin; goals; next_f; heur_f } =
    let find_ds_limited limit =
        (* smallest of the distances that go over the limit *)
        let next_limit = ref Int.max_int in
        let set_next_limit limit =
            if limit < !next_limit then next_limit := limit
        in
        (* add neighbours' properties to queue *)
        let add_to_queue queue path sum children =
            let transform ((self, dist) as node) =
                if List.mem self path then None (* prevent cycling *)
                else
                    let (dist, _, _) as node = sum + dist, node, self :: path in
                    let dist = dist + heur_f self in
                    if dist <= limit then
                        Some node
                    else
                        (set_next_limit dist; None)
            in
            List.filter_map transform children @ queue
        in
        let rec aux = function
            (* queue is exhausted, none of the goals found *)
            | [] -> Limit !next_limit
            (* there are still elements in the queue *)
            | (dist, (self, _), path) :: queue ->
                    (* goal found *)
                    if List.mem self goals then Result (List.rev path, dist)
                    else aux (add_to_queue queue path dist (next_f self))
        in
        aux [(0, (origin, 0), [origin])]
    in
    let rec find' limit =
        match find_ds_limited limit with
        | Result path_dist -> Some path_dist
        | Limit l when l <= limit -> None
        | Limit l -> find' l
    in
    find' (heur_f origin)
;;
assert (find_ida' { graph with heur_f = incorrect_h } = Some (['s'; 'b'; 'f'; 'n'; 'o'], 8));;
assert (find_ida' { graph with heur_f = correct_h   } = Some (['s'; 'b'; 'e'; 'l'], 7));;
assert (find_ida' { graph with heur_f = perfect_h   } = find_ida' { graph with heur_f = correct_h });;
assert (find_ida' { graph with heur_f = incorrect_h } = find_a' { graph with heur_f = incorrect_h });;
assert (find_ida' { graph with heur_f = correct_h   } = find_a' { graph with heur_f = correct_h   });;
assert (find_ida' { graph with heur_f = perfect_h   } = find_a' { graph with heur_f = perfect_h   });;
assert (find_ida' { graph with heur_f = (fun _ -> 0); goals = [' ']   } = None);;

