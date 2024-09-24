let grey_code n =
  let rec aux = function
    | 1 -> [ [ 0 ]; [ 1 ] ]
    | n ->
        let prev = aux (n - 1) in
        List.map (fun x -> 0 :: x) prev
        @ List.map (fun x -> 1 :: x) (List.rev prev)
  in
  aux n
