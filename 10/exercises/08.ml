(* #use "10/exercises/08.ml";; *)

type 'a graph = ('a * 'a) list

(* successori: 'a -> 'a graph -> 'a list *)
(* successori nodo g = lista dei successori di nodo in g (grafi orientato) *)
let rec successori nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest

(* test_connessi: 'a graph -> 'a -> 'a -> bool *)
(* test_connessi g start goal = verifica se esiste un cammino da start a gol in g*)
(* search: 'a list -> 'a list -> bool *)
(* search visited pending = (verifica che esiste un nodo raggiungibile da qualche nodo in pending mediante un cammino che non passa per visited) @ visisted *)
let test_connessi g start goal =
    let rec search visited = function
        | [] -> false
        | x::rest ->
            if List.mem x visited then search visited rest
            else x = goal || search (x::visited) ((successori x g) @ rest)
    in search [] [start]

(* connessi_in_glist: ('a * 'a) list list -> 'a -> 'a -> bool *)
(* connessi_in_glist glist start goal = verifica se almeno grafo g in glist ha un cammino da start a goal *)
let rec connessi_in_glist glist start goal =
    match glist with
        | [] -> false
        | g::gs ->
            test_connessi g start goal || connessi_in_glist gs start goal

let gs = [
    [
        (1,2);(1, 3);(1, 4);
        (2,6);
        (3,5);
        (4,6);
        (5,4);
        (6,5);(6,7)
    ];
    [
        (1,3);
        (2,6);
        (3,4);(3,5);(3,6);
        (4,2);(4,5);
        (5,4);
        (6,5)
    ]
]

let result = connessi_in_glist gs 1 7;;
