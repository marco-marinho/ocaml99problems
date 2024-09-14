let rec list_tail = function
  | [] -> None
  | t :: [] -> Some t 
  | _ :: t -> list_tail t

let intoption_to_int = function 
  | None -> 0
  | Some x -> x

let test_list = [1; 2; 3; 4; 5; 6]

let _ = print_string "List tail: ";; 
  print_int (intoption_to_int (list_tail test_list));; 
  print_string "\n";;

