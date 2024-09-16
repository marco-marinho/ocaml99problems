let encode lst =
  let rec aux acc count = function
    | [] -> []
    | [x] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
        if a == b then aux acc (count + 1) t
        else aux ((a, count + 1)::acc) 0 t
  in
  List.rev (aux [] 0 lst)

let print_tuple = function
  | (value, count ) -> (Printf.printf "(%s, %d) " value count)

let test_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let _ = List.iter print_tuple (encode test_list)
