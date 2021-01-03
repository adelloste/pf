(* #use "09/exercises/03.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* foglie_in_list: 'a list -> 'a ntree -> bool *)
let rec foglie_in_lista lst (Tr(x,tlist)) =
    match tlist with
        | [] -> List.mem x lst
        | _ ->
            fil_tlists lst tlist
and fil_tlists lst = function
    | [] -> true
    | t::ts ->
        foglie_in_lista lst t && fil_tlists lst ts;;

let lst = [3;5;6;7;9;10;11;12]

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

let result = foglie_in_lista lst tree;;
