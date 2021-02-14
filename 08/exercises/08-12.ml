(* #use "08/exercises/08-12.ml";; *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree
type 'a sostituzione = ('a * 'a tree) list

(* applica: 'a sostituzione -> 'a tree -> 'a list *)
let rec applica subst = function
    | Empty -> failwith "applica"
    | Tr(x,Empty,Empty) as t ->
        (try List.assoc x subst with _ -> t)
    | Tr(x,l,r) ->
        Tr(x,applica subst l,applica subst r)

let tree = Tr(
    10,
    Tr(8,Tr(6,Empty,Empty),Tr(4,Empty,Empty)),
    Tr(7,Tr(2,Empty,Empty),Tr(100,Empty,Empty))
)

let subst = [
    (100,Tr(101,Tr(102,Empty,Empty),Tr(103,Empty,Empty)))
]

let result = applica subst tree;;