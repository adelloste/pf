(* #use "others/partials/2019-prima-prova-parziale.ml";; *)

(* ======================================================================== *)
(* ================================= 1 ==================================== *)
(* ======================================================================== *)

(* f: ('a -> 'b -> bool) -> ('a * 'b) list -> 'a list *)
(* f p lst = lista di elementi in lst che soddisfano il predicato p *)
let rec f p = function
    | [] -> []
    | (x,y)::rest ->
        if p x y then x::f p rest
        else f p rest

let f p lst =
    let rec aux acc = function
        | [] -> List.rev acc
        | (x,y)::rest ->
            if p x y then aux (x::acc) rest
            else aux acc rest
    in aux [] lst

(* ======================================================================== *)
(* ================================= 2 ==================================== *)
(* ======================================================================== *)

(* compito: int list -> int list -> bool *)
(* compito prima seconda = verifica se per ogni elmento di prima esiste almeno un elemento di seconda che sia un suo divisore *)
let rec compito prima seconda =
    match prima with
        | [] -> true
        | x::rest ->
            List.exists (fun y -> x mod y = 0) seconda && compito rest seconda

let prima = [8;15;6]

let seconda = [3;4;5;6]

let result = compito prima seconda;;
