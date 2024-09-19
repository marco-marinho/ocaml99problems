let drop lst n =
  let rec aux acc n m = function
    | [] -> acc
    | h :: t -> if n == 0 then aux acc m m t else aux (h :: acc) (n - 1) m t
  in
  List.rev (aux [] (n - 1) (n - 1) lst)
