(* #use "09/exercises/04.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* num_di_foglie: 'a ntree -> int *)
let rec num_di_foglie (Tr(x,tlist)) =
    match tlist with
        | [] -> 1
        | _ ->
            num_di_foglie_tlist tlist
and num_di_foglie_tlist = function
    | [] -> 0
    | t::ts ->
        num_di_foglie t + num_di_foglie_tlist ts

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

let result = num_di_foglie tree;;
