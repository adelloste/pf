(* #use "10/exercises/09.ml";; *)

type 'a graph = ('a * 'a) list

let rec successori nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest

(* cammino_di_nodi: 'a graph -> 'a -> 'a list -> 'a list *)
let cammino_di_nodi g n l =
    let rec from_node visited a lst =
        if List.mem a visited then failwith "from_node"
        else if lst = [a] then [a]
        else
            a::(from_list (a::visited) (List.filter ((<>)a) lst) (successori a g))
    and from_list visited lst = function
        | [] -> failwith "from_list"
        | t::ts ->
            try from_node visited t lst
            with _ -> from_list visited lst ts
    in from_node [] n l

let g = [
    (1,2);(1, 3);(1, 4);
    (2,6);
    (3,5);
    (4,6);
    (5,4);
    (6,5);(6,7)
]

let result = cammino_di_nodi g 1 [2;5];;
