(* #use "09/exercises/02.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* postorder: 'a ntree -> 'a list *)
let rec postorder (Tr(x,tlist)) =
    match tlist with
        | [] -> [x]
        | _ ->
            postorder_tlist x tlist
and postorder_tlist x = function
    | [] -> [x]
    | t::ts ->
        (postorder t) @ (postorder_tlist x ts)

(* inorder: 'a ntree -> 'a list *)
let rec inorder = function
    | Tr(x,[]) -> [x]
    | Tr(x,t::ts) ->
        (inorder t) @ x::(List.flatten (List.map inorder ts))


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

let result_postorder = postorder tree

let result_inorder = inorder tree;;
