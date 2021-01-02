(* #use "others/exams/2014-febb.ml";; *)

(* ======================================================================== *)
(* ================================= 1 ==================================== *)
(* ======================================================================== *)

(* ordinati: 'a -> 'a list -> 'a list *)
let rec ordinati start = function
    | [] -> []
    | x::rest ->
        if x >= start then x::ordinati x rest
        else ordinati start rest

(* ======================================================================== *)
(* ================================= 2 ==================================== *)
(* ======================================================================== *)

type 'a ntree = Tr of 'a * 'a ntree list

(* livello: int -> 'a tree -> 'a list *)
let livello k t =
    let rec aux i (Tr(x,tlist)) =
        if i = k then [x]
        else aux_tlist (i + 1) tlist
    and aux_tlist i = function
        | [] -> []
        | t::ts ->
            aux i t @ aux_tlist i ts
    in aux 0 t

let t = Tr(10,[
    Tr(8,[
        Tr(20,[]);
        Tr(10,[
            Tr(5,[]);
            Tr(4,[])
        ]);
        Tr(20,[]);
    ]);
    Tr(5,[
        Tr(2,[
            Tr(1,[]);
            Tr(8,[])
        ]);
        Tr(8,[
            Tr(3,[]);
            Tr(10,[])
        ])
    ])
])

let result = livello 2 t

(* ======================================================================== *)
(* ================================= 3 ==================================== *)
(* ======================================================================== *)

type 'a graph = ('a * 'a) list

(* vicini: 'a -> 'a graph -> 'a list *)
(* vicini nodo g = lista dei vicini di nodo in g (grafo non orientato) *)
let rec vicini nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::vicini nodo rest
        else if y = nodo then x::vicini nodo rest
        else vicini nodo rest

(* path: 'a graph -> ('a -> bool) -> int -> 'a -> 'a list *)
let path g p k start =
    let rec from_node visited a i =
        if i < 0 || not (p a) || List.mem a visited then failwith "from_node"
        else if i = 0 then [a]
        else a::from_list (a::visited) (i - 1) (vicini a g)
    and from_list visited i = function
        | [] -> failwith "from_list"
        | t::ts ->
            try from_node visited t i
            with _ -> from_list visited i ts
    in from_node [] start (k - 1)

let g =  [
    (1,6);
    (2,1);(2,3);(2,4);
    (3,5);
    (4,1);(4,6);
    (5,4);(5,5);(5,6)
]

let result = path g (fun x -> x mod 2 = 0) 3 2;;

