(* #use "others/exams/2015-sett.ml";; *)

type 'a graph = ('a * 'a) list

(* ======================================================================== *)
(* ================================= A ==================================== *)
(* ======================================================================== *)

(* successori: 'a -> 'a graph -> 'a list *)
(* successori nodo g = lista dei successori di nodo in g (grafo orientato) *)
let rec successori nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest

(* ciclo: 'a graph -> 'a -> 'a list *)
(* ciclo g n = ciclo senza ripetizioni su n in g *)
(* from_node: 'a list -> 'a -> 'a list *)
(* from_node visited a ldl = cammino non ciclico da a fino al nodo n, che non passa da alcun nodo di visited *)
(* from_list 'a list -> 'a list *)
(* from_list visited list = cammino non ciclico da un nodo in list fino a n, che non passa da alcun nodo di visited *)
let ciclo g n =
    let rec from_node visited a =
        if List.mem a visited then failwith "ciclo"
        else if a = n then [a]
        else a::(from_list (a::visited) (successori a g))
    and from_list visited = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x with _ -> from_list visited rest
    in n::from_list [] (successori n g)

let g =  [
    (1,2);
    (2,3);
    (3,4);(3,6);
    (4,5);(4,7);
    (5,6);
    (6,1);(6,8);
    (7,6);
    (8,5);
]

let result = ciclo g 1

(* ======================================================================== *)
(* ================================= B ==================================== *)
(* ======================================================================== *)

(* ciclo_valido: 'a graph -> 'a list list -> 'a -> 'a list *)
(* ciclo_valido g lista_di_liste n = ciclo senza ripetizioni su n in g tale che ogni lista lista in lista_di_liste contenga almeno un nodo che occorre nel ciclo *)
(* from_node: 'a list -> 'a -> 'a list list *)
(* from_node visited a ldl = cammino non ciclico da a fino al nodo n contente i nodi in ldl, che non passa da alcun nodo di visited *)
(* from_list 'a list -> 'a list list -> 'a list *)
(* from_list visited n_ldl list = cammino non ciclico da un nodo in list fino a n contente i nodi in n_ldl, che non passa da alcun nodo di visited *)
let ciclo_valido g lista_di_liste n =
    let rec from_node visited a ldl =
        if List.mem a visited then failwith "from_node"
        else if a = n && ldl = [] then [a]
        else
            let n_ldl = List.filter (fun lst -> not(List.mem a lst)) ldl in
            a::from_list (a::visited) n_ldl (successori a g)
    and from_list visited n_ldl = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x n_ldl with _ -> from_list visited n_ldl rest
    in n::from_list [] lista_di_liste (successori n g)

let result = ciclo_valido g [[3;8];[7;5]] 1;;
