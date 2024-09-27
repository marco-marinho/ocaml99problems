type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let merge left right acc =
  List.fold_left
    (fun acc l ->
      List.fold_left (fun acc r -> Node ("x", l, r) :: acc) acc right)
    acc left

let rec cbal_tree n =
  if n = 0 then [ Empty ]
  else if n mod 2 = 1 then
    let t = cbal_tree (n / 2) in
    merge t t []
  else
    let t1 = cbal_tree ((n / 2) - 1) in
    let t2 = cbal_tree (n / 2) in
    merge t1 t2 (merge t2 t1 [])
