(* #use "10/exercises/02.ml";; *)

type 'a graph = ('a * 'a) list

(* successori: 'a -> 'a graph -> 'a list *)
(* successori n g = lista dei successori di n in g (grafo orientato) *)
let rec successori x = function
    | [] -> []
    | (i,j)::rest ->
        if i = x then j::successori x rest
        else successori x rest

(* esiste_ciclo: 'a graph -> 'a -> bool *)
(* esiste_ciclo g start = true sse un cammino da start a start (ciclo) altrimenti restituisce false *)
(* aux: 'a list -> 'a list -> bool *)
(* aux visited pending = true se da uno dei nodi in pending si può raggiungere start senza passare per i nodi di visited *)
let rec esiste_ciclo graph start =
    let rec aux visited = function
        | [] -> false
        | x::rest ->
            if List.mem x visited then aux visited rest
            else x = start || aux (x::visited) ((successori x graph) @ rest)
    in aux [] (successori start graph)

let graph = [(1,2);(1,3);(1,4);(2,3);(2,4);(2,7);(3,5);(4,6);(4,1);(4,7);(5,7);(6,7)]

let result = esiste_ciclo graph 1;;
