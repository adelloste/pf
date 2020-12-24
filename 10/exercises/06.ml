(* #use "10/exercises/06.ml";; *)

type 'a graph2 = 'a list * ('a * 'a) list

(* la funzione successori usata sotto e' quella definita
   all'inizio del file, e viene applicata alla componente
   archi del grafo *)

(* a -- cammino che passa per ogni nodo di lista esattamente 
        una volta *)
exception NotFound

(* cammino : 'a graph2 -> 'a list -> 'a -> 'a -> 'a list *)
(* from_node:'a -> 'a list -> 'a list
   from_node  x lista = cammino da x a goal che passa
       per tutti i nodi di lista esattamente una volta 
   from_list: 'a list -> 'a list -> 'a list
   from_list  lista listanodi = cammino da uno dei nodi
       di listanodi fino a goal, che passa per tutti i nodi
       di lista esattamente una volta *)
let cammino (nodi,archi) lista start goal =
  let rec from_node  x lista = 
    if not(List.mem x lista)
    then raise NotFound
    else 
      if x=goal && lista=[x] then [x]
      else let nuova=List.filter ((<>)x) lista
         (** si assume che la lista sia senza ripetizioni **)
      in x::from_list  nuova (successori x archi)
  and from_list  lista = function
      [] -> raise NotFound
    | x::rest ->
	try from_node  x lista
	with NotFound -> from_list  lista rest
  in from_node start lista

(** evidentemente, se in un grafo esiste un ciclo hamiltoniano,
   allora per qualsiasi nodo N esiste un cammino che parte
   da uno dei successori di N, e torna a N passando esattamente una
   volta per ciascun nodo del grafo **)

(* hamiltoniano: 'a graph -> 'a list *)
(* from_list: 'a list -> 'a list -> 'a list
   from_list lista listanodi = cammino da uno dei nodi in listanodi
     fino a n che passa per ciascun elemento di lista esattamente
     una volta *)
let hamiltoniano ((nodi,archi) as g) =
    let n = List.hd nodi in
    (* n e' un nodo qualsiasi *)
    let rec from_list lista = function
        | [] -> raise NotFound
        | x::rest ->
            try cammino g lista x n 
            with NotFound -> from_list lista rest 
    in n::from_list nodi (successori n archi)