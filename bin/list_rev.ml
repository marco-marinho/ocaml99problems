let list_rev lst =
  let rec list_rev_aux acc = function
    | [] -> acc
    | h :: t -> list_rev_aux (h :: acc) t
  in list_rev_aux [] lst

let test_list = [1;2;3;4;5]

let _ = List.iter (Printf.printf "%d,") (list_rev test_list);; print_endline "";;
