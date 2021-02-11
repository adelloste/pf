(* #use "08/exams/2019-sett.ml";; *)

(* complemento: 'a list -> 'a list -> 'a list *)
let rec complemento superset = function
    | [] -> superset
    | x::rest ->
        if List.mem x superset
        then complemento (List.filter ((<>) x) superset) rest
        else failwith "complemento"

let result = complemento [1;2;3;4;5;6] [2;4;6]

(* ======================================================================== *)
(* ============= Settembre 2019, riformulato per alberi binari ============ *)
(* ======================================================================== *)

exception NotFound

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* labels: 'a tree -> 'a list *)
(* labels t = lista delle etichette di tutti i nodi di t *)
let rec labels = function
    | Empty -> []
    | Tr(x,l,r) ->
        x::(labels l @ labels r)

(* discendenti: 'a -> 'a tree -> 'a list *)
let rec discendenti x = function
    | Empty -> []
    | Tr(y,Empty,Empty) ->
        if x = y then [y]
        else []
    | Tr(y,l,r) as t ->
        if y = x then labels t
        else discendenti x l @ discendenti x r;;

let tree = Tr(
    1,
    Tr(2,Tr(3,Empty,Empty),Tr(4,Tr(5,Empty,Empty),Tr(6,Empty,Empty))),
    Tr(3,Tr(8,Tr(9,Empty,Empty),Empty),Tr(2,Tr(4,Tr(3,Empty,Empty),Empty),Tr(3,Empty,Empty)))
)

let result = discendenti 2 tree;;
