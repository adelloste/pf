(* #use "09/exercises/05.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* listaguida: 'a list -> 'a ntree -> 'a *)
let rec listaguida lst (Tr(x,tlist)) =
    match lst with
        | [] -> x
        | x::rest ->
            try listaguida rest (List.nth tlist x)
            with _ -> failwith "listaguida"

let tree = Tr(
    1,
    [
        Tr(
            2,
            [ Tr(3,[ Tr(4,[]); Tr(5,[]) ]); Tr(6,[Tr(7,[]) ]); Tr(8,[] )] 
        );
        Tr(
            9,
            [ ]
        );
        Tr(
            10,
            [ Tr(11,[Tr(12,[]);Tr(13,[]);Tr(14,[])]);Tr(15,[]);Tr(16,[Tr(17,[]);Tr(18,[Tr(19,[]);Tr(20,[])])]) ]
        )
    ]
)

let result = listaguida [2;0] tree;;
