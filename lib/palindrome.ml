let list_rev lst =
  let rec list_rev_aux acc = function
    | [] -> acc
    | h :: t -> list_rev_aux (h :: acc) t
  in list_rev_aux [] lst

let is_palindrome lst =
  list_rev lst = lst



