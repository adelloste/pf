(* #use "09/exercises/07.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* tutte_foglie_costi: int ntree -> (int * int) *)
let rec tutte_foglie_costi (Tr(x,tlist)) =
    match tlist with
        | [] -> [(x,x)]
        | _ ->
            let subtrees = tutte_foglie_costi_tlist tlist in
            List.map (fun (k,v) -> (k,v+x)) subtrees
and tutte_foglie_costi_tlist = function
    | [] -> []
    | t::ts ->
        tutte_foglie_costi t @ tutte_foglie_costi_tlist ts

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

let result = tutte_foglie_costi tree;;
