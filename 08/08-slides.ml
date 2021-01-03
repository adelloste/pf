(* #use "08/08-slide.ml";; *)

(* ======================================================================== *)

type expr = 
    | Int of int
    | Var of string
    | Sum of expr * expr
    | Diff of expr * expr
    | Mult of expr * expr
    | Div of expr * expr 

(* eval: ambiente -> expr -> int *)
(* ambiente = [("x",5),("z",0),("y",4)] *)
let rec eval env = function
    | Int x -> x
    | Var x -> List.assoc x env
    | Sum (e1,e2) -> (eval env e1) + (eval env e2)
    | Diff (e1,e2) -> (eval env e1) - (eval env e2)
    | Mult (e1,e2) -> (eval env e1) * (eval env e2)
    | Div (e1,e2) -> (eval env e1) / (eval env e2)

(* alternative *)

type op = Sum | Diff | Mult | Div

type expr = Int of int | Var of string | Apply of op * expr * expr

(* opt2mat: op -> int -> int *)
let opt2map = function
    | Sum -> ( + )
    | Diff -> ( - )
    | Mult -> ( * )
    | Div -> ( / )

(* eval: (string * int) list -> expr -> int *)
let rec eval env = function
    | Int x -> x
    | Var x -> List.assoc x env
    | Apply (op,l,r) -> (opt2map op) (eval env l) (eval env r)

(* ======================================================================== *)
(* ================================ alberi ================================ *)
(* ======================================================================== *)

type 'a tree = Empty | Tr of 'a * 'a tree  * 'a tree

(* size: 'a tree -> int *)
(* size t = numero di nodi in t *)
let rec size = function
    | Empty -> 0
    | Tr(_,l,r) -> 1 + size l + size r

(* ======================================================================== *)

(* tree_exists: ('a -> bool) -> 'a tree -> bool *)
(* tree_exists p t = true sse t contiene almeno un nodo che soddisfa p *)

let rec tree_exists p = function
    | Empty -> false
    | Tr(x,l,r) -> 
        p x || tree_exists p l || tree_exists p r

(* ======================================================================== *)

(* raccogli: 'a tree -> 'a list *)
(* raccogli t = lista di tutte le etichette dei nodi di t (con ripetizioni) *)
let rec raccogli = function
    | Empty -> []
    | Tr(x,l,r) ->
        x::((raccogli l) @ (raccogli r))

(* ======================================================================== *)

(* foglie: 'a tree -> 'a list *)
(* foglie t = lista di tutte le etichette delle foglie di t (con ripetizioni) *)
let rec foglie = function
    | Empty -> []
    | Tr(x,Empty,Empty) -> [x]
    | Tr(x,l,r) -> (foglie l) @ (foglie r)

(* ======================================================================== *)

(* nodi_con_un_figlio: 'a tree -> 'a list *)
(* nodi_con_un_figlio t = lista di tutte le etichette dei nodi di t che hanno esattamente un  figlio *)
let rec nodi_con_un_figlio = function
    | Empty -> []
    | Tr(x,Empty,t) | Tr(x,t,Empty) -> t::nodi_con_un_figlio t
    | Tr(x,l,r) -> 
        (nodi_con_un_figlio l) @ (nodi_con_un_figlio r)
