(* #use "examples.ml";; *)

(* ======================================================================== *)
(* ============= Febbraio 2018, riformulato per alberi binari ============= *)
(* ======================================================================== *)

(* Ricerca di un cammino quindi utilizzo il backtracking *)

(* safe_path: int tree -> int -> int -> int list *)
(* safe_path t goal init = ... *)

exception NotFound

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let rec safe_path t goal init =
    match t with
        | Empty -> []
        | Tr(x,Empty,Empty) ->
            if x=goal then [x]
            else raise NotFound
        | Tr(x,l,r) ->
            let count = init + x in
            if count >= 0 
            then x::(try safe_path l goal count with NotFound -> safe_path r goal count)
            else raise NotFound;;

(* ================================ try it ================================ *)

let tree = Tr(
    3,
    Tr(-10,Tr(5,Tr(11,Empty,Empty),Tr(1,Empty,Empty)),Tr(2,Empty,Empty)),
    Tr(-1,Tr(1,Empty,Empty),Tr(-3,Empty,Empty))
)

safe_path tree 1 4


(* ======================================================================== *)
(* ============== Luglio 2018, riformulato per alberi binari ============== *)
(* ======================================================================== *)

(* depth: 'a -> 'a tree -> int *)
let rec depth n = function
    | Empty -> raise NotFound
    | Tr(x,Empty,Empty) ->
        if x = n then 0
        else raise NotFound
    | Tr(x,l,r) ->
        if x = n then 0
        else 1 + (try depth n l with NotFound -> depth n r)

(* ================================ try it ================================ *)

let tree = Tr(
    1,
    Tr(2,Tr(5,Empty,Empty),Tr(8,Tr(9,Tr(15,Empty,Empty),Empty),Tr(10,Empty,Empty))),
    Tr(3,Tr(6,Empty,Empty),Tr(7,Empty,Empty))
)

depth 15 tree


