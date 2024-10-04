type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let rec collect_leaves = function
  | Empty -> []
  | Node (_, Empty, Empty) as node -> [ node ]
  | Node (_, l, r) -> collect_leaves l @ collect_leaves r
