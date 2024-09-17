let split_list lst n =
  let rec aux acc n = function 
    | [] -> ((List.rev acc), [])
    | h :: t as l -> if n = 0 
    then ((List.rev acc), l)
    else aux (h :: acc) (n-1) t
  in
  aux [] n lst

