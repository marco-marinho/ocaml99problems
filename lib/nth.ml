let nth_excp = Failure "index out of range"

let rec nth lst n =
  match lst with
  | [] -> raise nth_excp
  | h :: t -> if n = 0 then h else nth t (n - 1)
