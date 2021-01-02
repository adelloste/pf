(* #use "10/exercises/06.ml";; *)

type 'a graph = 'a list * ('a * 'a) list

(* successori: 'a -> 'a graph -> 'a list *)
(* successori nodo g = lista dei successori di nodo in g (grafo orientato) *)
let rec successori nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest

(* ======================================================================== *)
(* ================================= a ==================================== *)
(* ======================================================================== *)

(* cammino: 'a graph -> 'a list -> 'a -> 'a -> 'a list *)
(* from_node: 'a -> 'a list -> 'a list *)
(* from_node x lst = cammino da x a goal che passa per tutti i nodi di list esattamente una volta *)
(* from_list: 'a list -> 'a list -> 'a list *)
(* from_list lst pending = cammino da uno dei nodi di pending fino a gola, che passa per tutti i nodi di lst esattamente una volta *)
let cammino (nodi,archi) lst start goal =
    let rec from_node  x lst = 
        if not(List.mem x lst) then failwith "from_node"
        else if x = goal && lst = [x] then [x]
        else x::from_list (List.filter ((<>)x) lst) (successori x archi)
    and from_list lst = function
        | [] -> failwith "from_list"
        | x::rest ->
	        try from_node x lst
	        with _ -> from_list lst rest
    in from_node start lst

(* ======================================================================== *)
(* ================================= b ==================================== *)
(* ======================================================================== *)

(* hamiltoniano: 'a graph -> 'a list *)
(* hamiltoniano g =  *)
(* search: 'a list -> 'a list -> 'a list *)
(* search lista listanodi = cammino da uno dei nodi in listanodi fino a n che passa per ciascun elemento di lista esattamente una volta *)
let hamiltoniano ((nodi,archi) as g) =
    let n = List.hd nodi in
    let rec search lst = function
        | [] -> failwith "search"
        | x::rest ->
            try cammino g lst x n
            with _ -> search lst rest
    in n::(search nodi (successori n archi));;
    