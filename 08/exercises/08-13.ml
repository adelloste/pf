(* #use "08/exercises/08-13.ml";; *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* path_corrente: 'a tree -> 'a list -> 'a list *)
let rec path_corrente t list =
    match t with
        | Empty -> failwith "path_corrente"
        | Tr(x,Empty,Empty) ->
            if list = [] || list = [x] then [x]else failwith "path_corrente"
        | Tr(x,l,r) ->
            let n_list = if List.mem x list then (List.filter ((<>) x) list) else list in
            x::(try path_corrente l n_list with _ -> path_corrente r n_list)

let tree = Tr(
    10,
    Tr(8,Tr(6,Empty,Empty),Tr(4,Empty,Empty)),
    Tr(7,Tr(2,Empty,Empty),Tr(100,Empty,Empty))
)

let list = [10;7;100]

let result = path_corrente tree list;;