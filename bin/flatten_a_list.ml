type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One(x) :: t -> aux (x :: acc) t
    | Many(x) :: t -> aux (aux acc x) t
  in
  List.rev (aux [] lst)

let test_lst = [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]

let _ = List.iter (print_string) (flatten test_lst)
