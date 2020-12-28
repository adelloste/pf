let rec n_ramo n (Tr(x,tlist)) =
    match tlist with
        | [] -> if x = n then [x] else failwith "n_ramo"
        | _ ->
            if (n - x) < 0 then failwith "n_ramo" else x::n_ramo_tlist (n - x) tlist
and n_ramo_tlist n = function
    | [] ->
    | t::ts ->
        try n_ramo n t with _ -> n_ramo_tlist n ts;;