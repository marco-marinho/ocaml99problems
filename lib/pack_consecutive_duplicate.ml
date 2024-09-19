let pack_consecutive_duplicates lst =
  let rec aux acc acc_curr = function
    | h :: t ->
        if acc_curr = [] || List.hd acc_curr = h then aux acc (h :: acc_curr) t
        else aux (acc_curr :: acc) [ h ] t
    | [] -> if acc_curr != [] then acc_curr :: acc else acc
  in
  List.rev (aux [] [] lst)
