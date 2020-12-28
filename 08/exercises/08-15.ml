(* #use "08-15.ml";; *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* abr_check: (’a * ’b) tree -> bool *)
let rec abr_check = function
    | Empty -> false
    | Tr(x,Empty,Empty) -> true
    | Tr(x,(Tr(a,_,_) as t),Empty) | Tr(x,Empty,(Tr(a,_,_) as t)) ->
        if x > a then abr_check t
        else failwith false
    | Tr(x,(Tr(a,_,_) as l),(Tr(b,_,_) as r)) ->
        if (x > a && x > b) then abr_check l && abr_check r
        else failwith false

let tree = Tr(
    10,
    Tr(8,Tr(6,Empty,Empty),Tr(4,Empty,Empty)),
    Tr(7,Tr(2,Empty,Empty),Tr(100,Empty,Empty))
)

let result = abr_check tree;;