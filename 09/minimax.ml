(* #use "09/minimax.ml";; *)

type player = Min | Max
type minmaxtree = Leaf of int | Node of (player * int) * minmaxtree list

(* minlist: 'a list -> 'a *)
(* minlist lst = elemento minimo nella lista lst *)
let rec minlist = function
    | [] -> failwith "minlist"
    | [x] -> x
    | x::y::rest ->
        minlist ((min x y)::rest)

(* maxlist: 'a list -> 'a *)
(* maxlist lst = elemento massimo nella lista lst *)
let rec maxlist = function
    | [] -> failwith "maxlist"
    | [x] -> x
    | x::y::rest ->
        maxlist ((max x y)::rest)

(* value: minmaxtree -> int *)
let value = function
    | Leaf n -> n
    | Node((_,n),_) -> n

(* propagate: minmaxtree -> minmaxtree *)
let rec propagate = function
    | Leaf n -> Leaf n
    | Node((p,n),tlist) ->
        let subtrees = propagate_tlist tlist in
        let lst = List.map value subtrees in
        if p = Min
        then Node((Min, (minlist lst) ), subtrees)
        else Node((Max, (maxlist lst) ), subtrees)
and propagate_tlist = function
    | [] -> []
    | t::ts ->
        [propagate t] @ propagate_tlist ts

let tree =
    Node(
        (Max,0),
        [
            Node((Min,100),[Leaf 3;Leaf 12;Leaf 8]);
            Node((Min,0),[Leaf 2;Leaf 4;Leaf 6]);
	        Node((Min,30),[Leaf 14;Leaf 15;Leaf 10])
        ]
    )

let result = propagate tree;;
