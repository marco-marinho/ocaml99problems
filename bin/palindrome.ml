let list_rev lst =
  let rec list_rev_aux acc = function
    | [] -> acc
    | h :: t -> list_rev_aux (h :: acc) t
  in list_rev_aux [] lst

let is_palindrome lst =
  list_rev lst = lst

let test_list = [1;2;3;2;1]

let test_list_false = [5;4;4;2;1]

let _ = Printf.printf "%B\n" (is_palindrome test_list)
let _ = Printf.printf "%B\n" (is_palindrome test_list_false)


