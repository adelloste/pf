(* #use "others/exams/2017-febb.ml";; *)

type 'a option = Some of 'a | None
type 'a graph = ('a * 'a) list

(* successori: 'a -> 'a graph -> 'a list *)
(* successori nodo g = lista dei successori di nodo in g (grafo orientato) *)
let rec successori nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest

(* checks: 'a -> 'a option -> bool *)
(* checks n options = true sse option è uguale a None oppure y è uguale a n *)
let checks n = function
    | Some(y) -> y = n
    | None -> true

(* whichpath: 'a graph -> 'a option list -> 'a -> 'a -> 'a list *)
(* from_node: 'a list -> 'a -> 'a option list *)
(* from_node visited a pl = cammino da a fino al nodo n, che è conforme con pl e parzialmente aciclico *)
(* from_list 'a list -> -> 'a option list -> 'a list *)
(* from_list visited pl list = cammino da un nodo in list fino a goal, che non passa da alcun nodo di visited *)
let whichpath g pattern_list start goal =
    let rec from_node visited a pl =
        if pl = [] then
            if List.mem a visited then failwith "from_node"
            else if a = goal then [a]
            else a::(from_list (a::visited) [] (successori a g))
        else
            if checks a (List.hd pl)
            then a::(from_list (a::visited) (List.tl pl) (successori a g))
            else failwith "from_node"
    and from_list visited pl = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x pl
            with _ -> from_list visited pl rest
    in from_node [] start pattern_list

let g =  [
    (1,2);(1,3);
    (2,5);
    (3,4);
    (4,2);(4,3);
    (5,5)
]

let result = whichpath g [None;None;None;None;None] 1 5;;
