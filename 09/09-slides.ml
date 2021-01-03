(* #use "09/09-slides.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* visita in preorder *)
(* preorder: 'a ntree -> 'a list *)
(* preorder t = lista dei nodi di t nell'ordine in cui sarebbero visitati secondo la visita in preordine *)
(* preorder_tlist: 'a ntree list -> 'a list *)
(* preorder_tlist [t1;...;t2] = (preorder t1) @ ... @ (preorder t2) *)

let rec preorder (Tr(x,tlist)) =
    x::preorder_tlist tlist
and preorder_tlist = function
    | [] -> []
    | t::rest ->
        preorder t @ preorder_tlist rest

let ntree =
    Tr(
        1,
        [
            Tr(2,[Tr(3,[Tr(4,[]);Tr(5,[])]);Tr(6,[Tr(7,[])]);Tr(8,[])]);
            Tr(9,[]);
        ]
    )

let result = preorder ntree

(* ======================================================================== *)

(* altezza di un albero *)

(* versione con mutua ricorsione *)
(* hl: 'a ntree list -> int *)
(* hl tlist = se tlist <>[] allora il massimo tra le h degli alberi in tlist, altrimenti errore *)
let rec h (Tr(x,tlist)) =
    match tlist with
        | [] -> 0
        | _ -> 1 + hl tlist 
    and hl = function
        | [] -> failwith "h"
        | [t] -> h t
        | t::rest -> max (h t) (hl rest);;

let result = h ntree

(* ======================================================================== *)

(* occurs_in: 'a ntree -> 'a -> bool *)
let rec occurs_in (Tr(x,tlist)) y =
    x = y || occurs_in_tlist tlist y
and occurs_in_tlist tlist y =
    match tlist with
        | [] -> false
        | t::rest ->
            occurs_in t y || occurs_in_tlist rest y

let result = occurs_in ntree 6

(* ======================================================================== *)

let rec depth x (Tr(y,tlist)) =
    if x = y then 0
    else 1 + depth_tlist x tlist
and depth_tlist x = function
    | [] -> failwith "depth"
    | t::rest ->
        try depth x t with _ -> depth_tlist x rest

(* albero binario *)
let rec depth x = function
    | Empty -> failwith "depth"
    | Tr(y,Empty,Empty) ->
        if x = y then 0
        else failwith "depth"
    | Tr(y,l,r) ->
        if x = y then 0
        else 1 + try depth x l with _ -> depth x r;;
