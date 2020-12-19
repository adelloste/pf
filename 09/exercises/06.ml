(* #use "09/exercises/06.ml";; *)

type 'a ntree = Tr of 'a * 'a ntree list

(* maxpair: ('a * 'a) list -> ('a * 'a) *)
let rec maxpair = function
    | [] -> failwith "maxpair"
    | [x] -> x
    | (x,y)::(z,w)::rest ->
        if y > w then maxpair ((x,y)::rest)
        else maxpair ((z,w)::rest)

(* foglia_costo: int ntree -> (int * int) *)
let rec foglia_costo (Tr(x,tlist)) =
    match tlist with
        | [] -> (x,x)
        | _ ->
            let subtrees = foglia_costo_tlist tlist in
            let (c,v) = maxpair subtrees in (c,v+x)
and foglia_costo_tlist = function
    | [] -> []
    | t::ts ->
        [foglia_costo t] @ foglia_costo_tlist ts

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

let result = foglia_costo tree;;
