let lst_length lst =
  let rec lst_length_aux lst acc =
    match lst with [] -> acc | _ :: t -> lst_length_aux t (acc + 1)
  in
  lst_length_aux lst 0
