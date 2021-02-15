(* #use "others/exams/2018-lugl.ml";; *)

(* ======================================================================== *)
(* ================================== 3 =================================== *)
(* ======================================================================== *)

type 'a ntree = Tr of 'a * 'a ntree list

(* depth: 'a -> 'a ntree -> int *)
(* depth n t = profondità di n in t *)
let rec depth n (Tr(x,tlist)) =
    match tlist with
        | [] ->
            if n = x then 0
            else failwith "depth_tlist"
        | _ ->
            if n = x then 0
            else 1 + depth_tlist n tlist
and depth_tlist n = function
    | [] -> failwith "depth_tlist"
    | t::ts ->
        try depth n t with _ -> depth_tlist n ts;;

(* parentela: 'a ntree -> 'a -> 'a -> int *)
let rec parentela (Tr(x,tlist) as t) a b =
    try parentela_tlist a b tlist
    with _ -> depth a t + depth b t
and parentela_tlist a b = function
    | [] -> failwith "parentela"
    | t::ts ->
        try parentela t a b with _ -> parentela_tlist a b ts

let ntree = 
    Tr(
        1,
        [
            Tr(2, [  Tr(5,[]); Tr(8, [  Tr(9,[ Tr(15,[]) ]); Tr(10,[]) ]) ]);
            Tr(3,[ Tr(6,[]); Tr(7,[]); Tr(18,[ Tr(29,[]); Tr(4,[]) ]) ]);
            Tr(14,[ Tr(19,[]); Tr(12,[]); Tr(29,[ Tr(13,[]) ]) ])
        ]
    )

let result = parentela ntree 15 13;;
