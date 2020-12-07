(* #use "2018-lugl.ml";; *)

(* ======================================================================== *)
(* ============== Luglio 2018, riformulato per alberi binari ============== *)
(* ======================================================================== *)

exception NotFound

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* ================================= ex 2 ================================= *)

(* depth: 'a -> 'a tree -> int *)
let rec depth n = function
    | Empty -> raise NotFound
    | Tr(x,Empty,Empty) ->
        if x = n then 0
        else raise NotFound
    | Tr(x,l,r) ->
        if x = n then 0
        else 1 + (try depth n l with _ -> depth n r)

(* ================================ try it ================================ *)

let tree = Tr(
    1,
    Tr(2,Tr(5,Empty,Empty),Tr(8,Tr(9,Tr(15,Empty,Empty),Empty),Tr(10,Empty,Empty))),
    Tr(3,Tr(6,Empty,Empty),Tr(7,Empty,Empty))
)

let result = depth 15 tree;;

(* =============================== ex 3 - TODO =============================== *)