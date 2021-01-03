(* #use "09/exercises/12.ml";; *)

type col = Rosso | Giallo | Verde | Blu
type 'a col_assoc = (col * 'a list) list
type 'a ntree = Tr of 'a * 'a ntree list

(* tcolore: 'a -> 'a col_assoc -> col *)
let rec tcolore x = function
    | [] -> failwith "tcolore"
    | (c,lst)::rest ->
        if List.mem x lst then c
        else tcolore x rest;;

let rec ramo_colorato x lst_col_assoc (Tr(y,tlist)) =
    match tlist with
        | [] -> if x = y then [y] else failwith "ramo_colorato"
        | _ ->
            let pcolor = tcolore y lst_col_assoc in
            y::rct x lst_col_assoc ( List.filter (fun (Tr(x,_)) -> (tcolore x lst_col_assoc) <> pcolor) tlist )
and rct x lst_col_assoc = function
    | [] -> failwith "ramo_colorato"
    | t::ts ->
        try ramo_colorato x lst_col_assoc t with _ -> rct x lst_col_assoc ts;;

let lst = [(Rosso,[1;5]); (Giallo,[2;7;11;12]); (Verde,[4;6;10]); (Blu,[3;8;9])]

let tree = Tr(
    1,
    [
        Tr(
            2,
            [ Tr(3,[]);Tr(4,[Tr(5,[]);Tr(6,[])]);Tr(4,[Tr(7,[])]); ]
        );
        Tr(
            3,
            [ Tr(8,[Tr(9,[]);Tr(10,[])]); Tr(2,[Tr(4,[Tr(3,[])]);Tr(11,[]);Tr(12,[])]) ]
        )
    ]
)

let result = ramo_colorato 3 lst tree;;
