(* #use "09/exams/2018-lugl.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

let rec depth x (Tr(y,tlist)) =
    match tlist with
        | [] ->
            if x = y then 0
            else failwith "depth"
        | _ ->
            if x = y then 0
            else 1 + depth_tlist x tlist
and depth_tlist x = function
    | [] -> failwith "depth_tlist"
    | t::ts ->
        try depth x t with _ -> depth_tlist x ts;;

let tree = Tr(
    1,
    [
        Tr(
            2,
            [ Tr(5,[]);Tr(8,[Tr(9,[Tr(15,[])]);Tr(10,[])]) ]
        );
        Tr(
            3,
            [ Tr(6,[]);Tr(7,[]);Tr(18,[Tr(29,[]);Tr(4,[])]) ]
        );
        Tr(
            14,
            [ Tr(19,[]);Tr(12,[]);Tr(29,[Tr(13,[])]) ]
        )
    ]
)

let result = depth 1 tree;;
