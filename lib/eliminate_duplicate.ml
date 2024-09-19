let eliminate_duplicates lst =
  let rec aux acc ilst current =
    match (ilst, current) with
    | [], _ -> acc
    | h :: t, None -> aux (h :: acc) t (Some h)
    | h :: t, Some x ->
        if x = h then aux acc t (Some h) else aux (h :: acc) t (Some h)
  in
  List.rev (aux [] lst None)
