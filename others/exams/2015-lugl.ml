(* #use "others/exams/2015-lugl.ml";; *)

type 'a tree = Tr of 'a * 'a tree list

(* ======================================================================== *)
(* ================================= A ==================================== *)
(* ======================================================================== *)
(* radici: 'a tree list -> 'a list *)
(* radici m = *)
(* aux: 'a list -> 'a tree list -> 'a list *)
(* aux acc m =  *)
let radici m =
    let rec aux acc = function
        | [] -> acc
        | (Tr(x,_))::ts -> 
            if List.mem x acc then aux acc ts
            else aux (x::acc) ts
    in aux [] m

(* alternative mood *)
let rec radici = function
    | [] -> []
    | Tr(x,_)::ts ->
        x::radici ts

let t = [
    Tr(1,[ Tr(2,[ Tr(5,[]) ]) ]);
    Tr(8,[ Tr(9,[ Tr(5,[])]); Tr(10,[]) ]);
    Tr(7,[]);
    Tr(8,[Tr(9,[]); Tr(4,[])]);
    Tr(4,[Tr(9,[])]);
    Tr(10,[]);
    Tr(9,[Tr(2,[])])
]

let result = radici t

(* ======================================================================== *)
(* ================================= B ==================================== *)
(* ======================================================================== *)

(* archi_t: 'a tree -> ('a * 'a) list *)
(* archi_t t = lista di coppie di archi in t *)
(* archi_tlist: 'a tree list -> ('a * 'a) list *)
(* archi_tlist ts = lista di coppie dei sottoalberi di ts *)
let rec archi_t (Tr(x,tlist)) =
    match tlist with
        | [] -> []
        | _ ->
            (List.map (fun y -> (x,y)) (radici tlist)) @ archi_tlist tlist
and archi_tlist = function
    | [] -> []
    | t::ts ->
        archi_t t @ archi_tlist ts

(* del: ('a * 'a) list -> ('a * 'a) list *)
(* del lst = lista di coppie di archi senza ripetizioni *)
let rec del = function
    | [] -> []
    | (x,y)::rest ->
        if List.mem (x,y) rest then del rest
        else (x,y)::del rest

let t = Tr(1,[
    Tr(2,[Tr(5,[]);Tr(8,[Tr(9,[Tr(5,[])]);Tr(10,[])])]);
    Tr(3,[Tr(6,[]);Tr(7,[]);Tr(8,[Tr(9,[]); Tr(4,[])])]);
    Tr(4,[Tr(9,[]);Tr(10,[]);Tr(9,[Tr(2,[])])])
])

let result = del (archi_t t);;
