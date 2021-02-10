(* #use "08/exams/2019-lugl.ml";; *)

(* ======================================================================== *)
(* ================================== 1 =================================== *)
(* ======================================================================== *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

let root = function
    | Empty -> failwith "root"
    | Tr(x,_,_) -> x

(* n_ramo_bin: int -> int tree -> int list *)
(* n_ramo_bin n tree = cammino dalla root a una foglia tale che la somma di ogni nodo è uguale a n *)
let rec n_ramo_bin n = function
    | Empty -> failwith "n_ramo_bin"
    | Tr(y,Empty,Empty) ->
        if y = n then [y]
        else failwith "n_ramo_bin"
    | Tr(y,t,Empty) | Tr(y,Empty,t) ->
        if (y + root t) <= n then y::(n_ramo_bin (n - y) t)
        else failwith "n_ramo_bin"
    | Tr(y,l,r) ->
        try
            if (y + root l) <= n then y::(n_ramo_bin (n - y) l)
            else failwith "n_ramo_bin"
        with _ -> 
            if (y + root r) <= n then y::(n_ramo_bin (n - y) r)
            else failwith "n_ramo_bin"

let tree = Tr(
    5,
    Tr(7,Tr(3,Empty,Empty),Tr(9,Empty,Empty)),
    Tr(3,Tr(7,Empty,Empty),Tr(10,Empty,Empty))
)

let result = n_ramo_bin 18 tree

(* ======================================================================== *)
(* ============== Luglio 2019, riformulato per alberi binari ============== *)
(* ======================================================================== *)

exception NotFound

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* int -> int tree -> int list *)
let n_ramo_bin mx t =
    (* aux: 'a -> 'a tree -> 'a list *)
    (* aux padre t = path dal nodo padre al nodo con etichetta mx *)
    let rec aux padre = function
        | Empty -> raise NotFound
        | Tr(x,Empty,Empty) ->
            if (x + padre) = mx then [x]
            else raise NotFound
        | Tr(x,l,r) ->
            let sum = padre + x in
            if sum < mx then x::(try aux sum l with _ -> aux sum r)
            else raise NotFound
    in aux 0 t;;

let tree = Tr(
    5,
    Tr(2,Tr(5,Empty,Empty),Tr(10,Empty,Empty)),
    Tr(3,Tr(2,Empty,Empty),Tr(10,Empty,Empty))
)

let result = n_ramo_bin 10 tree;;
