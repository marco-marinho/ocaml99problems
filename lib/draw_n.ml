let draw n m =
  let rec aux acc = function
    | 0 -> acc
    | n ->
        let el = Random.int m in
        if List.mem el acc then aux acc n else aux (el :: acc) (n - 1)
  in
  aux [] n
