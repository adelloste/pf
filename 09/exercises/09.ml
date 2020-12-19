(* #use "09/exercises/09.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* primo: 'a -> bool *)
let primo n =
    let rec aux = function
        | 1 -> true
        | k -> (n mod k)<>0 && aux (k-1)
    in n>0 && (n=1 || aux (n/2))

(* ramo_di_primi: int ntree -> int *)
let rec ramo_di_primi (Tr(x,tlist)) =
    match tlist with
        | [] ->
            if (primo x) then x
            else failwith "ramo_di_primi"
        | _ -> 
            if (primo x) then ramo_di_primi_tlist tlist
            else failwith "ramo_di_primi"
and ramo_di_primi_tlist = function
    | [] -> failwith "ramo_di_primi_tlist"
    | t::ts ->
        try ramo_di_primi t with _ -> ramo_di_primi_tlist ts

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

let result = ramo_di_primi tree;;

