(* #use "10/exams/2020-febb.ml";; *)

type 'a graph = ('a * 'a * string) list

(* ======================================================================== *)
(* ================================= 1 ==================================== *)
(* ======================================================================== *)

(* add: 'a list -> 'a list -> 'a list *)
(* add acc lst = lista degli elementi presenti in acc con gli elementi di lst sse non sono già presenti *)
let rec add acc = function
    | [] -> acc
    | x::rest ->
        if List.mem x acc then add acc rest
        else add (x::acc) rest

(* line: metro -> string -> int list *)
(* line m ln = lista delle stazioni per le quali passa la linea ln *)
(* aux: 'a list -> metro -> 'a list *)
(* aux acc m = lista delle stazioni per le quali passa la linea ln (da verificare)  *)
let line m ln =
    let rec aux acc = function
        | [] -> acc
        | (x,y,z)::rest ->
            if z = ln then aux (add acc [x;y]) rest
            else aux acc rest
    in aux [] m

let m = [
    (1,2,"A");
    (2,3,"A");(2,4,"B");
    (3,1,"A");
    (4,5,"B");(4,6,"C");
    (5,7,"D");
    (6,3,"C");(6,7,"D")
]

let result = line m "D"

(* ======================================================================== *)
(* ================================= 2 ==================================== *)
(* ======================================================================== *)

(* vicini: int -> metro -> (int * string) list *)
(* vicini stazione metro = lista dei vicini di stazione in metro (grafo non orientato) *)
let rec vicini stazione = function
    | [] -> []
    | (x,y,z)::rest ->
        if x = stazione then (y,z)::vicini stazione rest
        else if y = stazione then (x,z)::vicini stazione rest
        else vicini stazione rest

let m = [
    (1,2,"A");
    (2,3,"A");(2,4,"B");
    (3,1,"A");
    (4,5,"B");(4,6,"C");
    (5,7,"D");
    (6,3,"C");(6,7,"D")
]

let result = vicini 7 m

(* ======================================================================== *)
(* ================================= 3 ==================================== *)
(* ======================================================================== *)

(* raggiungi: 'a graph -> int -> int -> int -> int list *)
(* raggiungi m maxc start goal = lista dei nodi da start a goal in g dove il numero dei cambi è minore o uguale a maxc *)
(* from_node: int -> string -> int list -> int -> int list *)
(* from_node cambi linea visited a = cammino che non passa per i nodi in visited, da a fino al nodo goal dove cambi deve essere minore o uguale a maxc *)
let raggiungi m maxc start goal =
    let rec from_node cambi linea visited a =
        if List.mem a visited then failwith "from_node"
        else if a = goal && cambi <= maxc then [a]
        else a::(from_list (a::visited) cambi linea (vicini a m))
    and from_list visited cambi linea = function
        | [] -> failwith "from_list"
        | (x,y)::rest ->
            let n_cambi = if linea = y then cambi else (cambi + 1) in
            let n_linea = if linea = y then linea else y in
            try from_node n_cambi n_linea visited x with _ -> from_list visited n_cambi n_linea rest
    in from_node (-1) "" [] start

let m = [
    (1,2,"A");
    (2,3,"A");(2,4,"B");
    (3,1,"A");
    (4,5,"B");(4,6,"C");
    (5,7,"D");
    (6,3,"C");(6,7,"D")
]

let result = raggiungi m 2 6 1;;