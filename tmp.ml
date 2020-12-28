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






(* ======================================== *)

type player = Min | Max
type minmaxtree = Leaf of int | Node of (player * int) * minmaxtree list

(* VISITA IN POSTORDINE *)
let rec propagate = function
    | Leaf n -> [n]
    | Node((p,n),tlist) ->
        propagate_tlist tlist @ [n]
and propagate_tlist = function
    | [] -> []
    | t::ts ->
        propagate t @ propagate_tlist ts;;


let tree =
    Node(
        (Max,0),
        [
            Node((Min,100),[Leaf 3;Leaf 12;Leaf 8]);
            Node((Min,0),[Leaf 2;Leaf 4;Leaf 6]);
	        Node((Min,30),[Leaf 14;Leaf 15;Leaf 10])
        ]
    )

(* minlist: 'a list -> 'a
   minlist lst = elemento minimo in lst *)
let rec minlist = function
    | [] -> failwith "minlist"
    | [x] -> x
    | x::y::rest -> minlist ((min x y)::rest)

(* maxlist: 'a list -> 'a
   maxlist lst = elemento massimo in lst *)
let rec maxlist = function
    | [] -> failwith "maxlist"
    | [x] -> x
    | x::y::rest -> maxlist ((max x y)::rest)

(* value: minmaxtree -> int
   value t = valore numerico della radice di t *)
let value = function
    | Leaf n -> n
    | Node((_,n),_) -> n

let rec propagate = function
    | Leaf n -> Leaf n
    | Node((p,n),tlist) ->
        let lst = propagate_tlist tlist in
        let tmp = List.map value lst in
        if p = Min
        then Node((Min, (minlist tmp) ), lst)
        else Node((Max, (maxlist tmp) ), lst)
and propagate_tlist = function
    | [] -> []
    | t::ts ->
        [propagate t] @ propagate_tlist ts;;
        (* Node( (Min,100), ([propagate t] @ propagate_tlist ts) );; *)


        (* Node((p,v),subtrees) *)