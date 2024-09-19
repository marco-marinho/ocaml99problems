let perm lst =
  let rec pop acc n = function
    | [] -> raise Not_found
    | h :: t -> if n = 0 then (h, acc @ t) else pop (h :: acc) (n - 1) t
  in
  let rec aux acc n = function
    | [] -> acc
    | l ->
        let el, lst = pop [] (Random.int n) l in
        aux (el :: acc) (n - 1) lst
  in
  aux [] (List.length lst) lst
