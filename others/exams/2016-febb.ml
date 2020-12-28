(* #use "others/exams/2016-febb.ml";; *)

type 'a action_graph = ('a * string * 'a) list

(* ======================================================================== *)
(* ================================= A ==================================== *)
(* ======================================================================== *)

(* successori: 'a -> 'a action_graph -> (string * 'a) list *)
(* successori nodo g = lista dei successori di nodo in g (grafo orientato) *)
let rec successori nodo = function
    | [] -> []
    | (x,y,z)::rest ->
        if x = nodo then (y,z)::successori nodo rest
        else successori nodo rest

(* ======================================================================== *)
(* ================================= C ==================================== *)
(* ======================================================================== *)

(* move: 'a action_graph -> 'a -> 'a -> string list *)
let move g start goal =
    let rec from_node visited a =
        if List.mem a visited then failwith "from_node"
        else if a = goal then []
        else
            from_list (a::visited) (successori a g)
    and from_list visited = function
        | [] -> failwith "from_list"
        | (x,y)::rest ->
            try x::(from_node visited y) with _ -> from_list visited rest
    in from_node [] start

let g =  [
    (1,"a",2);(1,"b",3);(1,"c",4);
    (2,"a",6);
    (3,"b",5);(3,"c",5);
    (4,"b",1);(4,"c",6);
    (5,"c",4);(5,"a",5);(5,"b",5);
    (6,"b",5)
]

let result = move g 3 6;;
