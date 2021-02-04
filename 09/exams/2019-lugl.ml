(* #use "09/exams/2019-lugl.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* n_ramo: int -> int ntree -> int list *)
let rec n_ramo n (Tr(x,tlist)) =
    match tlist with
        | [] -> if x = n then [x] else failwith "n_ramo" 
        | _ ->
            x::n_ramo_tlist (n - x) ( List.filter (fun (Tr(y,_)) -> x + y <= n) tlist )
and n_ramo_tlist n = function
    | [] -> failwith "n_ramo_tlist"
    | t::ts ->
        try n_ramo n t with _ -> n_ramo_tlist n ts

let tree = Tr(
    5,
    [
        Tr(
            7,
            [ Tr(3,[]);Tr(9,[]);Tr(12,[]); ]
        );
        Tr(
            1,
            [ Tr(4,[]);Tr(5,[]);Tr(10,[]) ]
        );
        Tr(
            3,
            [ Tr(9,[]);Tr(7,[]);Tr(10,[]) ]
        )
    ]
)

let result = n_ramo 18 tree;;