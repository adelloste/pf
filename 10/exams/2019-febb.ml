(* #use "10/exams/2019-febb.ml";; *)

type color = Rosso | Verde | Neutro
type 'a graph = ('a * 'a) list

(* vicini: 'a -> 'a graph -> 'a list *)
(* vicini nodo g = lista dei vicini di nodo in g (grafo non orientato) *)
let rec vicini nodo = function
    | [] -> []
    | (x,y)::rest ->
        if x = nodo then y::vicini nodo rest
        else if y = nodo then x::vicini nodo rest
        else vicini nodo rest

(* path: 'a graph -> ('a * color) list -> color list -> 'a -> 'a list *)
(* path g colors lst start = cammino non ciclico in g che parte da start e rispetta la lista lst *)
(* from_node: 'a list -> 'a -> color list *)
(* from_node visited a lst = cammino non ciclico che rispetta la lista lst, che non passa da alcun nodo di visited *)
(* from_list 'a list -> 'a list -> 'a list *)
(* from_list visited lst list = cammino non ciclico da uno dei nodi in list che rispetta lst e non passa per alcun numero in visited *)
let path g colors lst start =
    let rec from_node visited a lst =
        let c = List.assoc a colors in
        if List.mem a visited then failwith "from_node"
        else if [c] = lst || lst = [] then [a]
        else
            let n_lst = if List.hd lst = c then List.tl lst else lst in
            a::(from_list (a::visited) n_lst (vicini a g))
    and from_list visited lst = function
        | [] -> failwith "from_list"
        | x::rest ->
            try from_node visited x lst with _ -> from_list visited lst rest
    in from_node [] start lst

let g = [
    (1,2);(1,3);
    (2,3);(2,5);
    (3,4);(3,5);
    (4,5);
    (5,6);(5,7);
    (6,7);
    (7,8)
]

let colors = [
    (1,Neutro);
    (2,Rosso);
    (3,Verde);
    (4,Verde);
    (5,Neutro);
    (6,Verde);
    (7,Rosso);
    (8,Neutro)
]

let result = path g colors [Rosso;Verde;Neutro] 1;;
