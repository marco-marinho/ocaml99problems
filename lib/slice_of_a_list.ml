let slice start stop lst =
  let rec aux acc n = function
    | [] -> List.rev acc
    | h :: t ->
        if n > stop then List.rev acc
        else if n >= start then aux (h :: acc) (n + 1) t
        else aux acc (n + 1) t
  in
  aux [] 0 lst
