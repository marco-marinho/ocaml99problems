let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | Types.OneNode(x) :: t -> aux (x :: acc) t
    | Types.ManyNode(x) :: t -> aux (aux acc x) t
  in
  List.rev (aux [] lst)
