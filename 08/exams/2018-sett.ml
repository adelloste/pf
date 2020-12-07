(* #use "2018-sett.ml";; *)

(* ======================================================================== *)
(* =========================== Settembre 2018 ============================= *)
(* ======================================================================== *)

exception NotFound

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* sorted_branch: 'a tree -> 'a -> 'a list *)
let sorted_branch t x =
    (* aux: 'a -> 'a tree -> 'a list *)
    (* aux padre t = path dal nodo padre al nodo con label x *)
    let rec aux padre = function
        | Empty -> raise NotFound
        | Tr(y,Empty,Empty) ->
            if y = x && y >= padre then [y]
            else raise NotFound
        | Tr(y,l,r) ->
            if y >= padre then y::(try aux y l with _ -> aux y r)
            else raise NotFound
    in match t with
        | Empty -> raise NotFound
        | Tr(y,Empty,Empty) ->
            if y = x then [y]
            else raise NotFound
        | Tr(y,l,r) ->
            y::(try aux y l with _ -> aux y r);;

(* ================================ try it ================================ *)

let tree = Tr(
    1,
    Tr(2,Tr(1,Empty,Empty),Tr(3,Tr(4,Empty,Empty),Tr(5,Empty,Empty))),
    Tr(3,Tr(6,Empty,Empty),Tr(7,Empty,Empty))
)

let result = sorted_branch tree 4

(* ======================================================================== *)
(* ========================== Altra soluzione ============================= *)
(* ======================================================================== *)

(* radice: 'a tree -> 'a *)
(* radice t = restituisce il node radice dell'albero t *)
let radice = function 
    | Empty -> failwith "radice"
    | Tr(a,_,_) -> a 

let rec sorted_branch tree x =
    match tree with
        | Empty -> failwith "sorted_branch"
        | Tr(a,Empty,Empty) ->
            if a = x then [a]
            else failwith "sorted_branch"
        | Tr(a,t,Empty) | Tr(a,Empty,t) ->
            if a <= radice t
            then a::sorted_branch t x
            else raise Not_found
        | Tr(a,l,r) ->
            try
                if a > radice l then failwith "sorted_branch"
                else a:: sorted_branch l x
            with _ ->
                if a > radice r then failwith "sorted_branch"
                else a:: sorted_branch r x;;
