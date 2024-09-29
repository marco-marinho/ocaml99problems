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

let rec is_mirror n1 n2 =
  match (n1, n2) with
  | Empty, Empty -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> is_mirror l1 r2 && is_mirror l2 r1
  | _ -> false

let is_symmetric = function Empty -> true | Node (_, l, r) -> is_mirror l r
let sym_bal_trees n = List.filter is_symmetric (cbal_tree n)
