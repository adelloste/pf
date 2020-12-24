(* #use "10/exercises/11.ml";; *)

type 'a graph = ('a * 'a) list

(* successori: 'a -> 'a graph -> 'a list *)
(* successori nodo g = lista dei successori di nodo in g (grafo orientato) *)
let rec successori nodo = function
  | [] -> []
  | (x,y)::rest ->
    if x = nodo then y::successori nodo rest
    else successori nodo rest

(* primo: int -> bool *)
(* primo n = true sse n è divisibile per 1 e per se stesso, altrimenti 0 *)
(* aux: int -> bool *)
(* aux n = n non e' divisibile per alcun numero compreso tra 2 e x (inclusi) *)
let primo n =
  let rec aux = function
    | 1 -> true
    | x -> (n mod x <> 0) && aux (x - 1)
  in n = 1 || aux (n/2)

(* cammino_di_primi: 'a graph -> 'a -> 'a -> 'a list *)
(* cammino_di_primi g start goal = cammino da start a goal di numeri primi *)
(* from_node: 'a list -> 'a -> 'a list *)
(* from_node visited a = cammino non ciclico da a a goal che passa solo per numeri primi e non passa per alcun numero in visited *)
(* from_list 'a list -> 'a list -> 'a list *)
(* from_list visited list = cammino non ciclico da uno dei nodi in lista fino a goal che passa solo per numeri primi e non passa per alcun numero in visited *)
let cammino_di_primi g start goal =
  let rec from_node visited a =
    if List.mem a visited || not(primo a) then failwith "from_node"
    else if a = goal then [a]
    else a::(from_list (a::visited) (successori a g))
  and from_list visited = function
    | [] -> failwith "from_list"
    | x::rest ->
      try from_node visited x with _ -> from_list visited rest
  in from_node [] start

let g = [
  (1,2);(1,3);(1,4);(1,5);
  (2,3);(2,4);(2,6);
  (3,4);
  (4,6);
  (5,4);(5,6)
]

let result = cammino_di_primi g 1 5;;