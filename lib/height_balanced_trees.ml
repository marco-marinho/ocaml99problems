type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let merge left right acc =
  List.fold_left
    (fun acc l ->
      List.fold_left (fun acc r -> Node ("x", l, r) :: acc) acc right)
    acc left

let rec chbal_tree = function
  | 0 -> [ Empty ]
  | 1 -> [ Node ("x", Empty, Empty) ]
  | n ->
      let t = chbal_tree (n - 1) in
      let t2 = chbal_tree (n - 2) in
      merge t t (merge t2 t (merge t t2 []))
