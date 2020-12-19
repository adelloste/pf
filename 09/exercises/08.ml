(* #use "09/exercises/08.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

let rec ramo_da_lista (Tr(x,tlist)) list k =
    match tlist with
        | [] ->
            if x = k && list = [x] then [x]
            else failwith "ramo_da_lista"
        | _ ->
            if List.mem x list
            then x::(ramo_da_lista_tlist (List.filter ((<>) x) list)) k tlist
            else failwith "ramo_da_lista"
and ramo_da_lista_tlist lst k = function
    | [] -> failwith "ramo_da_lista_tlist"
    | t::ts ->
        try ramo_da_lista t lst k with _ -> ramo_da_lista_tlist lst k ts

let lst = [10;1;14;11]

let tree = Tr(
    1,
    [
        Tr(
            2,
            [ Tr(3,[ Tr(4,[]); Tr(5,[]);]); Tr(6,[ Tr(7,[]); ]); Tr(8,[]); ]
        );
        Tr(
            9,
            []
        );
        Tr(
            10,
            [ Tr(11,[Tr(12,[]); Tr(13,[]); Tr(14,[]);]); Tr(15,[]); Tr(16,[Tr(17,[]); Tr(18,[ Tr(19,[]); Tr(20,[]);]);]); ]
        )
    ]
)

let result = ramo_da_lista tree lst 14;;

