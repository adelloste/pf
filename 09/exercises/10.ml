(* #use "09/exercises/10.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* path_non_pred: ('a -> bool) -> 'a ntree -> 'a list *)
let rec path_non_pred p (Tr(x,tlist)) =
    match tlist with
        | [] -> 
            if p x then failwith "path_non_pred"
            else [x]
        | _ -> 
            if p x then failwith "path_non_pred"
            else x::pnpt p tlist
and pnpt p = function
    | [] -> failwith "pnpt"
    | t::ts ->
        try path_non_pred p t with _ -> pnpt p ts

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
            3,
            [ Tr(11,[Tr(12,[]); Tr(13,[]); Tr(14,[]);]); Tr(15,[]); Tr(5,[Tr(7,[]); Tr(18,[ Tr(19,[]); Tr(20,[]);]);]); ]
        )
    ]
)

let result = path_non_pred (fun x -> x > 1000) tree;;
