(* complemento: 'a list -> 'a list -> 'a list *)
let rec complemento superset = function
    | [] -> superset
    | x::rest ->
        if List.mem x superset
        then complemento (List.filter ((<>) x) superset) rest
        else failwith "complemento"

let result = complemento [1;2;3;4;5;6] [2;4;6]

(* =================================== Febb 2017 ===================================== *)

type color = Rosso | Verde | Neutro

let conta_colori cols list =
    (* aux: int -> int -> int -> 'a list -> (color * int) list *)
    (* aux r g n list = data list restituisce una lista delle occorrenze dei color r g n *)
    let rec aux r g n = function
        | [] -> [(Rosso,r);(Verde,g);(Neutro,n)]
        | x::rest ->
            try
                let v = List.assoc x cols in
                if v = Rosso
                then aux (r+1) g n rest
                else aux r (g+1) n rest
            with
                _ -> aux r g (n+1) rest
    in aux 0 0 0 list;;

let result = conta_colori [(2,Rosso);(3,Verde);(4,Verde);(6,Verde);(7,Rosso)] [1;2;3;4;5;6;7;8;9;10]

(* =================================== Lugl 2017 ===================================== *)

let rec some_all p = function
    | [] -> failwith "some_all"
    | x::rest ->
        if List.for_all (fun y -> p y) x then x
        else some_all p rest

let result = some_all (function x -> x mod 2 = 0) [[1;3;5];[2;4;6];[1;4];[1;10]]
