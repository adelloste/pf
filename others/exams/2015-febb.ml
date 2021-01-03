(* #use "others/exams/2015-febb.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* ======================================================================== *)
(* ================================ 1.A =================================== *)
(* ======================================================================== *)

(* mkset: 'a list -> 'a list *)
(* mkset lst = lista di nodi senza duplicati *)
let rec mkset = function
    | [] -> []
    | x::rest -> 
        if List.mem x rest then mkset rest
        else x::mkset rest

(* preorder: 'a ntree -> 'a list *)
(* preorder t = lista dei nodi in t *)
(* preorder_tlist: 'a ntree list -> 'a list *)
(* preorder_tlist [t1;...;tn] = (preorder t1) @ ... @ (preorder tn) *)
let rec preorder (Tr(x,tlist)) =
    x::preorder_tlist tlist
and preorder_tlist = function
    | [] -> []
    | t::ts ->
        preorder t @ preorder_tlist ts

(* nodi:'a ntree -> 'a list *)
(* nodi t = lista dei nodi in t senza duplicati *)
let nodi t = mkset (preorder t)

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

let result = nodi t

(* ======================================================================== *)
(* ================================ 1.B =================================== *)
(* ======================================================================== *)

(* preorder_f: 'a -> 'a ntree -> 'a list *)
(* preorder_f n t = lista dei figli di n in t *)
(* preorder_f_tlist: 'a -> 'a ntree list -> 'a list *)
(* preorder_f_tlist [t1;...;tn] = (preorder_f t1) @ ... @ (preorder_f tn) *)
let rec preorder_f n (Tr(x,tlist)) =
    if x = n then (List.map (fun (Tr(x,_)) -> x) tlist)
    else preorder_f_tlist n tlist
and preorder_f_tlist n = function
    | [] -> []
    | t::ts ->
        preorder_f n t @ preorder_f_tlist n ts

(* figli: 'a -> 'a ntree -> 'a list *)
(* figli n t = lista dei figli di n in t senza duplicati *)
let figli n t = mkset (preorder_f n t)

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

let result = figli 8 t

(* ======================================================================== *)
(* ================================= 2 ==================================== *)
(* ======================================================================== *)

type 'a graph = ('a * 'a * int) list

(* successori: 'a -> 'a graph -> 'a list *)
(* successori nodo g = lista dei successori di nodo in g (grafo orientato) *)
let rec successori nodo = function
    | [] -> []
    | (x,y,z)::rest ->
        if x = nodo then (y,z)::successori nodo rest
        else successori nodo rest

let wpath g start goal pesomax =
    let rec from_node visited a peso =
        if List.mem a visited then failwith "from_node"
        else if a = goal && peso <= pesomax then ([a],peso)
        else
            let (path,p) = from_list (a::visited) peso (successori a g)
            in (a::path,p)
    and from_list visited p = function
        | [] -> failwith "from_list"
        | (x,y)::rest ->
            try
                from_node visited x (p+y)
            with _ -> from_list visited p rest
    in from_node [] start 0

let g =  [
    ('A','B',2);('A','D',1);
    ('B','B',1);('B','C',1);('B','E',8);
    ('C','A',3);('C','D',3);('C','E',5);
    ('D','C',6);('D','E',10)
]

let result = wpath g 'A' 'E' 100;;
