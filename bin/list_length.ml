let lst_length lst =
  let rec lst_length_aux lst acc =
    match lst with
    | [] -> acc
    | _ :: t -> lst_length_aux t (acc + 1)
  in lst_length_aux lst 0

let _ = print_int (lst_length [1;2;3;4;5]);; print_endline "";;

let _ = print_int (lst_length []);; print_endline "";;
