  let rec insert el n = function 
    | [] -> []
    | h :: t -> if n = 0 then el :: h :: t else h :: insert el (n - 1) t
