(* #use "2019-lugl.ml";; *)

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
