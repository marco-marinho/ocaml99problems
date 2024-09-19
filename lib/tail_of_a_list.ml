let rec list_tail = function
  | [] -> None
  | t :: [] -> Some t
  | _ :: t -> list_tail t

let intoption_to_int = function None -> 0 | Some x -> x
