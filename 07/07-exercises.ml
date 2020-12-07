(* ======================================================================== *)
(* ================================ 07 - 1 ================================ *)
(* ======================================================================== *)

type direzione = Su | Giu | Destra | Sinistra
type posizione = int * int * direzione
type azione = Gira | Avanti of int

let gira = function
    | Su -> Destra
    | Giu -> Sinistra
    | Destra -> Giu
    | Sinistra -> Su

let avanti (x,y,dir) n =
    match dir with
        | Su -> (x,y+n,dir)
        | Giu -> (x,y-n,dir)
        | Destra -> (x+n,y,dir)
        | Sinistra -> (x-n,y,dir)

let sposta (x,y,dir) act =
    match act with
        | Gira -> (x,y,gira dir)
        | Avanti n -> avanti (x,y,dir) n

(* esergui: posizione -> azione list -> posizione *)
let rec esegui p = function
    | [] -> p
    | x::rest -> esegui (sposta p x) rest

(* ======================================================================== *)
(* ================================ 07 - 2 ================================ *)
(* ======================================================================== *)

type chiave = Aperta | Chiusa
type cassaforte = chiave list

let gira = function
    | Aperta -> Chiusa
    | Chiusa -> Aperta

let rec giradopochiusa = function
    | [] -> failwith "giradopochiusa"
    | x::y::rest -> 
        if x = Chiusa then x::(gira y)::rest
        else x::giradopochiusa(y::rest)
    | x::rest -> [x]

giradopochiusa [Aperta;Aperta;Aperta;Aperta;Aperta;Aperta;Chiusa]

(* ======================================================================== *)
(* ================================ 07 - 4 ================================ *)
(* ======================================================================== *)

type obj = Miss | Cann | Barca
type situazione = obj list * obj list

(* count: obj list -> int * int *)
let rec count = function
    | [] -> (0,0)
    | x::rest ->
        let (l,r) = count rest in
        match x with
            | Cann -> (l+1,r)
            | Miss -> (l,r+1)
            | _ -> (l,r)

(* safe: situazione -> bool *)
let safe (l,r) =
    let (cl,ml) = count l in cl <= ml && let (cr,mr) = count r in cr <= mr





