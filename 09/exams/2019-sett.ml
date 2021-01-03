(* #use "09/exams/2019-sett.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* labels: 'a ntree -> 'a list *)
(* labels t = la lista di tutti i nodi appartenenti all'albero t *)
let rec labels (Tr(x,tlist)) =
    match tlist with
        | [] -> [x]
        | _ -> [x] @ labels_tlist tlist
and labels_tlist = function
    | [] -> []
    | t::ts ->
        labels t @ labels_tlist ts

(* discendenti: 'a -> 'a ntree -> 'a list *)
let rec discendenti y (Tr(x,tlist) as t) =
    match tlist with
        | [] -> if x = y then [x] else []
        | _ ->
            if x = y then labels t
            else discendenti_tlist y tlist
and discendenti_tlist y = function
    | [] -> []
    | t::ts ->
        discendenti y t @ discendenti_tlist y ts

let tree = Tr(
    1,
    [
        Tr(
            2,
            [ Tr(3,[]);Tr(4,[Tr(5,[]);Tr(6,[])]);Tr(4,[Tr(7,[])]); ]
        );
        Tr(
            3,
            [ Tr(8,[Tr(9,[]);Tr(10,[])]); Tr(2,[Tr(4,[Tr(3,[])]);Tr(11,[]);Tr(12,[])]) ]
        )
    ]
)

let result = discendenti 2 tree;;
