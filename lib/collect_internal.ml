type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let rec collect_internal = function
  | Node (_, l, r) as node -> node :: (collect_internal l @ collect_internal r)
  | _ -> []