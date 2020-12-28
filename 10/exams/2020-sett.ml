(* #use "10/exams/2020-sett.ml";; *)

type 'a graph = ('a * 'a) list

(* successori: 'a -> 'a graph -> 'a list *)
(* successori nodo g = lista dei successori di nodo in g (grafo orientato) *)
let rec successori nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest

(* percorso: 'a graph -> 'a -> 'a -> 'a -> 'a list *)
(* percorso g start tappa target =  *)
(* from_node: 'a list -> 'a -> color list *)
(* from_node visited a = cammino non ciclico da a fino al nodo target contente tappa, che non passa da alcun nodo di visited *)
(* from_list 'a list -> 'a list -> 'a list *)
(* from_list visited list = cammino non ciclico da un nodo in list fino a target contente tappa, che non passa da alcun nodo di visited *)
let percorso g start tappa target =
    let rec from_node visited a =
        if List.mem a visited then failwith "from_node"
        else if a = target && (a = tappa || List.mem tappa visited) then [a]
        else a::(from_list (a::visited) (successori a g))
    and from_list visited = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x with _ -> from_list visited rest
    in from_node [] start

let g = [
    (1,2);(1,3);(1,4);(1,5);
    (2,10);
    (3,2);(3,7);
    (4,3);(4,7);(4,8);
    (5,4);(5,9);(5,10);
    (6,2);
    (7,6);(7,10);
    (8,10);
    (9,10);
]

let result = percorso g 1 5 10;;
