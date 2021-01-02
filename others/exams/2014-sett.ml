(* #use "others/exams/2014-sett.ml";; *)

(* ======================================================================== *)
(* ================================= 1 ==================================== *)
(* ======================================================================== *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* rami: 'a tree -> 'a list list *)
(* ramo t = lista contenente tutti i rami in t *)
let rec rami = function
    | Empty -> []
    | Tr(x,Empty,Empty) -> [[x]]
    | Tr(x,l,r) ->
        List.map (fun y -> x::y) (rami l @ rami r)

let t = Tr(
    1, 
    Tr(
        2, 
        Tr(3, Empty, Empty),
        Empty
    ),
    Tr(
        4,
        Tr(
            5,
            Tr(3, Empty, Empty),
            Empty
        ),
        Tr(
            2,
            Empty,
            Tr(6, Empty, Empty)
        )
    )
)

let result = rami t

(* ======================================================================== *)
(* ================================= 2 ==================================== *)
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

(* pathwith: 'a -> 'a graph -> 'a -> 'a -> 'a list *)
let pathwith n g start goal =
    let rec from_node visited a =
        if List.mem a visited then failwith "from_node"
        else if a = goal && (List.mem n visited || a = n) then [a]
        else a::from_list (a::visited) (vicini a g)
    and from_list visited = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x
            with _ -> from_list visited rest
    in from_node [] start


let g = [
    (1,2);(1,3);
    (2,4);(2,5);
    (3,4);
    (4,5)
]

let result = pathwith 3 g 2 5;;
