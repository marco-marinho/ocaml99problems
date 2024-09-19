  let combi lst k =
    let rec aux acc cur n = function
      | [] -> if n = 0 then [List.rev cur] else []
      | h :: t ->  if n = 0 then ((List.rev cur) :: acc) else
      aux acc (h :: cur) (n - 1) t @ aux acc cur n t
    in
    aux [] [] k lst
