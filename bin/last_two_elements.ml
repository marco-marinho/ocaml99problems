let rec last_two_elements = function 
  | [] -> None
  | _ :: [] -> None
  | f :: s :: [] -> Some (f, s)
  | _ :: t -> last_two_elements t

let inttupleoption_to_int = function
  | None -> (0 , 0)
  | Some(x, y) -> (x, y)

let test_list = [1;2;3;4;5;6]

let _ = print_string "List last two: ";;
  let (x, y) = (inttupleoption_to_int (last_two_elements test_list)) in
  Printf.printf "%d, %d\n%!" x y
