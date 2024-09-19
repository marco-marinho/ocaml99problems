let replicate lst n =
  let rec aux acc n m = function
    | [] -> acc
    | h :: t ->
        if n > 0 then aux (h :: acc) (n - 1) m (h :: t) else aux acc m m t
  in
  List.rev (aux [] n n lst)
