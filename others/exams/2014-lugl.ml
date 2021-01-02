(* #use "others/exams/2014-lugl.ml";; *)

(* ======================================================================== *)
(* ================================= 1 ==================================== *)
(* ======================================================================== *)

(* ======================================================================== *)
(* ================================= 2 ==================================== *)
(* ======================================================================== *)

(* listfrom: 'a -> 'a list -> 'a list *)
let rec listfrom y = function
    | [] -> failwith "fromlist"
    | x::rest ->
        if x = y then x::rest
        else listfrom y rest

(* ======================================================================== *)
(* ================================= 3 ==================================== *)
(* ======================================================================== *)

type 'a ntree = Tr of 'a * 'a ntree list

(* ramociclico: 'a ntree -> 'a list *)
let ramociclico t =
    let rec aux visited (Tr(x,tlist)) =
        match tlist with
            | [] ->
                if List.mem x visited then [x]
                else failwith "ramociclico"
            | _ ->
                if List.mem x visited then [x]
                else x::aux_tlist (x::visited) tlist
    and aux_tlist visited = function
        | [] -> failwith "aux_tlist"
        | t::ts ->
            try aux visited t
            with _ -> aux_tlist visited ts
    in aux [] t

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

let result = ramociclico t

(* ======================================================================== *)
(* ================================= 4 ==================================== *)
(* ======================================================================== *)

type 'a graph = ('a * 'a) list

let rec successori nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::successori nodo rest
        else successori nodo rest

(* ciclo: 'a graph -> 'a -> 'a list *)
let ciclo g start =
    let rec from_node visited a =
        if List.mem a visited then (listfrom a (List.rev visited) @ [a])
        else from_list (a::visited) (successori a g)
    and from_list visited = function
        | [] -> failwith "from_list"
        | t::ts ->
            try from_node visited t
            with _ -> from_list visited ts
    in from_node [] start

let g = [(1,2);(2,3);(3,4);(4,5);(5,6);(6,3)]

let result = ciclo g 1;;