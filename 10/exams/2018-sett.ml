(* #use "10/exams/2018-sett.ml";; *)

type 'a graph = ('a * 'a) list

(* successori: 'a -> 'a graph -> 'a list *)
(* successori nodo g = lista dei successori di nodo in g (grafo orientato) *)
let rec successori nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest

(* sorted_path: 'a graph -> 'a -> 'a -> 'a list' *)
(* sorted_path g start goal = lista ordinata di nodi da start a goal in g *)
(* from_node: 'a list -> 'a -> 'a -> 'a list *)
(* from_node visited a = cammino ordinato che non passa per nodi in visited, da a fino al nodo goal *)
let sorted_path g start goal =
    let rec from_node visited a =
        if List.mem a visited then failwith "from_node"
        else if a = goal then [a]
        else a::( from_list (a::visited) (List.filter (fun x -> x >= a) (successori a g)))
    and from_list visited = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x with _ -> from_list visited rest
    in from_node [] start

let g = [
    (1,2);(1,3);(1,4);
    (2,6);
    (3,5);
    (4,6);
    (5,4);
    (6,5);(6,7)
]

let result = sorted_path g 1 7;;
