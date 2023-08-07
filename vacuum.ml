open Graph

type sesalec = Left | Right;;
type cleaned = Dirty | Clean;;
type rooms = cleaned * cleaned;;
type state = rooms * sesalec;;

let vacuum = {
    origin =
        (Dirty, Dirty), Left;
    goals = [
        (Clean, Clean), Left;
        (Clean, Clean), Right
    ];
    next_f = (function state ->
        let suck = function
            | (Dirty, Dirty), Left -> (Clean, Dirty), Left
            | (Dirty, Dirty), Right -> (Dirty, Clean), Right
            | (Dirty, Clean), Left -> (Clean, Clean), Left
            | (Clean, Dirty), Right -> (Clean, Clean), Right
            | (Clean, _), Left as state -> state
            | (_, Clean), Right as state -> state
        and move = function
            | Right, (rooms, Left) -> rooms, Right
            | Left, (rooms, Right) -> rooms, Left
            | _, state -> state
        in
        [suck state, 1; move (Left, state), 1; move (Right, state), 1]
    );
    heur_f = (function (rooms, _) ->
        match rooms with
        | (Dirty, Dirty) -> 2
        | (Dirty, Clean)
        | (Clean, Dirty) -> 1
        | (Clean, Clean) -> 0
    );
    to_string = (function ((l, r), loc) ->
        (match loc with
        | Left  -> "v  "
        | Right -> "  v")
        ^ "\n"
        ^ (match l with
        | Dirty -> "@"
        | Clean -> "_")
        ^ " "
        ^ (match r with
        | Dirty -> "@"
        | Clean -> "_")
    )
}
;;

match find_a' vacuum with
| Some (path, steps) ->
        print_string @@ string_of_path vacuum.to_string path;
        Format.printf "steps: %d\n" steps
| None -> print_endline "not found"

