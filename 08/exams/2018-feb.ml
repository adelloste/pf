(* #use "08/exams/2018-feb.ml";; *)

(* ======================================================================== *)
(* ============= Febbraio 2018, riformulato per alberi binari ============= *)
(* ======================================================================== *)

(* Ricerca di un cammino quindi utilizzo il backtracking *)

exception NotFound

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* safe_path: int tree -> int -> int -> int list *)
(* safe_path t goal init = ramo dell'albero t dalla radie a una foglia goal *)
let rec safe_path t goal init =
    match t with
        | Empty -> raise NotFound
        | Tr(x,Empty,Empty) ->
            if x = goal && (init + x) >= 0 then [x]
            else raise NotFound
        | Tr(x,l,r) ->
            let count = init + x in
            if count >= 0 
            then x::(try safe_path l goal count with _ -> safe_path r goal count)
            else raise NotFound;;

let tree = Tr(
    3,
    Tr(-10,Tr(5,Tr(11,Empty,Empty),Tr(1,Empty,Empty)),Tr(2,Empty,Empty)),
    Tr(-1,Tr(1,Empty,Empty),Tr(-3,Empty,Empty))
)

let result = safe_path tree 1 4;;
