  let get n lst = 
    let rec aux acc n = function
      | [] -> acc
    | l -> if n = 0 then acc 
    else aux (List.nth l (Random.int (List.length l)) :: acc) (n-1) l
    in
    aux [] n lst
