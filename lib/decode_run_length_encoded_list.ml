let decode lst =
  let mutiple x n = 
    let rec aux acc x = function
      | 0 -> acc
      | n -> aux (x :: acc) x (n-1)
    in
    aux [] x n
  in
    let rec aux acc = function
      | [] -> acc
      | Types.One(x) :: t -> aux (x :: acc) t
      | Types.Many(c, x) :: t -> aux ((mutiple x c) @ acc) t
    in
  List.rev (aux [] lst)

