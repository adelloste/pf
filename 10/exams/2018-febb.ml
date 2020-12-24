(* #use "10/exams/2018-febb.ml";; *)

type 'a graph = ('a * 'a) list
type 'a money = ('a * int) list

(* vicini: 'a -> 'a graph -> 'a list *)
(* vicini nodo g = lista dei vicini di nodo in g (grafo non orientato) *)
let rec vicini nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::vicini nodo rest
        else if y = nodo then x::vicini nodo rest
        else vicini nodo rest

(* accessibili: ('a * int) list -> int -> ('a * 'a) list -> 'a list *)
(* accessibili wallet v g = lista dei nodi raggiungibili in g il cui valore t meno v è maggiore o uguale a 0 *)
let rec accessibili wallet v = function
    | [] -> []
    | x::rest ->
        let t = try List.assoc x wallet with _ -> 0 in
        if v - t >= 0 then x::accessibili wallet v rest
        else accessibili wallet v rest

(* safe_path: ('a * 'a) list -> ('a * int) list -> 'a -> 'a -> init *)
(* safe_path g wallet start goal init = lista dei nodi da start a goal in g dove la somma dei valori di ogni nodo, partendo da init, è maggiore o uguale a 0 *)
(* from_node: 'a list -> 'a -> 'a -> 'a list *)
(* from_node visited a counter = cammino che non passa per nodi in visited, da a fino al nodo goal, dove la somma dei valori di ogni nodo, partendo da init, è maggiore o uguale a 0 *)
let safe_path g wallet start goal init =
    let rec from_node visited a counter =
        let v = try List.assoc a wallet with _ -> 0 in
        if List.mem a visited then failwith "from_node"
        else if a = goal && (counter - v) >= 0 then [a]
        else a::( from_list (a::visited) (counter - v) (accessibili wallet (counter - v) (vicini a g)) )
    and from_list visited counter = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x counter with _ -> from_list visited counter rest
    in from_node [] start init

let wallet = [
    ('C',-7);
    ('D',-15);
    ('F',3);
    ('G',-5)
]

let g = [
    ('A','B');('A','C');('A','D');
    ('B','E');
    ('C','E');('C','F');
    ('D','F');
    ('E','G');
    ('F','G');
]

let result = safe_path g wallet 'A' 'G' 10;;
