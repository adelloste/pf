(* #use "08/exercises/08-14.ml";; *)

(* ======================================================================== *)
(* ================================= a ==================================== *)
(* ======================================================================== *)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list

(* colore: 'a -> 'a col_assoc -> col *)
let rec colore x = function
    | [] -> failwith "colore"
    | (c,list)::rest ->
        if List.mem x list then c
        else colore x rest

let lst = [(Rosso,[1;2;4;7;10]); (Giallo,[3;8;11]); (Verde,[0;5;6;13]); (Blu,[9;12;14;15;100])]

let result = colore 6 lst

(* ======================================================================== *)
(* ================================= b ==================================== *)
(* ======================================================================== *)

type 'a tree = Empty | Tr of 'a * 'a tree * 'a tree

(* radice: 'a col_assoc -> 'a tree -> 'a' *)
(* radice colori t = colore del nodo radice in t *)
let radice colori = function
    | Empty -> failwith "colori"
    | Tr(x,_,_) -> colore x colori

(* path_to: 'a -> 'a col_assoc -> 'a tree -> 'a list *)
let rec path_to x colori = function
    | Empty -> failwith "path_to"
    | Tr(y,Empty,Empty) ->
        if y = x then [y]
        else failwith "path_to"
    | Tr(y,l,r) ->
        let cy = colore y colori in
        try
            if cy <> (radice colori l) then y::path_to x colori l
            else failwith "path_to"
        with _ ->
            if cy <> (radice colori r) then y::path_to x colori r
            else failwith "path_to"

let tree = Tr(
    10,
    Tr(8,Tr(6,Empty,Empty),Tr(4,Empty,Empty)),
    Tr(7,Tr(2,Empty,Empty),Tr(100,Empty,Empty))
)

let result = path_to 4 lst tree;;
