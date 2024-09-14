let nth_excp = Failure "index out of range"

let rec nth lst n =
  match lst with
  | [] -> raise nth_excp
  | h :: t -> if n = 0 then h else nth t (n-1)

let test_list = [1;2;3;4;5;7]

let _ = Printf.printf "%d \n" (nth test_list 4)

let _ = Printf.printf "%d \n" (nth test_list 10)
