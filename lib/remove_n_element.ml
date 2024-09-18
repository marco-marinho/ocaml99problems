  let rec remove n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: (remove (n-1) t)
