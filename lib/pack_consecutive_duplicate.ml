let pack_consecutive_duplicates lst =
  let rec aux acc acc_curr = function
    | h :: t -> if acc_curr = [] || (List.hd acc_curr) = h then aux acc (h :: acc_curr) t else aux (acc_curr :: acc) [h] t
    | [] -> if acc_curr != [] then acc_curr :: acc else acc
  in
  List.rev (aux [] [] lst)

let test_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
let res = (pack_consecutive_duplicates test_list)
let _ = List.iter (List.iter print_string) res
