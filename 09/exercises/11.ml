(* #use "09/exercises/11.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* same_structure: 'a ntree -> 'b ntree -> bool *)
let rec same_structure (Tr(x,tlist)) (Tr(y,tlist2)) =
    same_list tlist tlist2
and same_list tlist tlist2 =
    match (tlist,tlist2) with
        | ([],[]) -> true
        | (t::rest,t2::rest2) ->
            same_structure t t2 && same_list rest rest2

let tree1 = Tr(
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

let tree2 = Tr(
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

let result = same_structure tree1 tree2;;
