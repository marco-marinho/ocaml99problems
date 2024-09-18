  let rotate n lst =
    let rec aux m acc = function
      | [] -> List.rev acc
      | h :: t as l -> if m >= n then l @ (List.rev acc) 
      else aux (m + 1) (h :: acc) t
    in
    aux 0 [] lst
      
