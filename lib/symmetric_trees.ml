type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let rec is_mirror n1 n2 =
  match (n1, n2) with
  | Empty, Empty -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror l2 r1
  | _ -> false

let is_symmetric = function Empty -> true | Node (_, l, r) -> is_mirror l r
