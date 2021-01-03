(* #use "10/exercises/13.ml";; *)

type 'a graph = ('a * 'a) list

let path_n_p g p x start =
  let rec from_node counter visited a =
    if List.mem a visited then failwith "from_node"
    else if p a && counter = 1 then [a]
    else
      let c = if p a then (counter - 1) else counter in
      a::(from_list c (a::visited) (successori a g))
    and from_list counter visited = function
      | [] -> failwith "from_list"
      | x::rest ->
        try from_node counter visited x with _ -> from_list counter visited rest
    in from_node x [] start

let pari x = x mod 2 = 0

let g = [
  (1,3);
  (2,6);
  (3,4);(3,5);(3,6);
  (4,2);(4,5);
  (5,4);
  (6,5)
]

let result = path_n_p g pari 2 1;;
