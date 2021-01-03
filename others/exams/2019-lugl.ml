(* #use "others/exams/2019-lugl.ml";; *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* ======================================================================== *)
(* ================================= 1 ==================================== *)
(* ======================================================================== *)

(* radice: 'a tree -> 'a *)
(* radice t = radice dell'albero t *)
let radice = function
    | Empty -> failwith "radice"
    | Tr(x,_,_) -> x

(* n_ramo_bin: int -> int tree -> int list *)
let n_ramo_bin n t =
    let rec aux padre = function
        | Empty -> failwith "n_ramo_bin"
        | Tr(x,Empty,Empty) ->
            if (x + padre) = n then [x]
            else failwith "n_ramo_bin"
        | Tr(x,l,r) ->
            try
                if x + radice l > n then failwith "n_ramo_bin"
                else x::aux (x + padre) l
            with _ ->
                if x + radice r > n then failwith "n_ramo_bin"
                else x::aux (x + padre) r
    in aux 0 t

let t = Tr(
    5,
    Tr(1,Tr(5,Empty,Empty),Tr(10,Empty,Empty)),
    Tr(3,Tr(7,Empty,Empty),Tr(10,Empty,Empty))
)

let result = n_ramo_bin 18 t;;
