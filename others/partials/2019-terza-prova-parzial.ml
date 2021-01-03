(* #use "others/partials/2019-terza-prova-parzial.ml";; *)

(* ======================================================================== *)
(* ================================= 1 ==================================== *)
(* ======================================================================== *)

type 'a graph = ('a * 'a) list

(* vicini: 'a -> 'a graph -> 'a list *)
(* vicini nodo g = lista dei vicini di nodo in g (grafo non orientato) *)
let rec vicini nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::vicini nodo rest
        else if y = nodo then x::vicini nodo rest
        else vicini nodo rest

(* compito: int graph -> int -> int -> int -> int * int list *)
(* from_node: int list -> int -> int -> int * int list *)
(* from_node visited a c = cammino non ciclico da a fino al nodo goal il cui costo è minore o uguale a k, che non passa da alcun nodo di visited *)
(* from_list: int list -> int -> int list -> int * int list *)
(* from_list visited c list = cammino non ciclico da un nodo in list fino a goal il cui costo è minore o uguale a k, che non passa da alcun nodo di visited *)
let compito g start goal k =
    let rec from_node visited a c =
        if List.mem a visited || c > k then failwith "from_node"
        else if a = goal && c <= k then (c,[a])
        else
            let (c,p) = from_list (a::visited) c (vicini a g) in (c,a::p)
    and from_list visited c = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x (c + x)
            with _ -> from_list visited c rest
    in from_node [] start start

let g = [
    (2,30);(2,4);(4,8);(5,30);(10,20);(10,5);(10,2);(20,4);(30,8)
]

let result = compito g 10 8 25;;
