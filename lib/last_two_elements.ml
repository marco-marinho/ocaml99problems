let rec last_two_elements = function
  | [] -> None
  | _ :: [] -> None
  | [ f; s ] -> Some (f, s)
  | _ :: t -> last_two_elements t

let inttupleoption_to_int = function None -> (0, 0) | Some (x, y) -> (x, y)
