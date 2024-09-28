type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let rec insert x = function
  | Empty -> Node (x, Empty, Empty)
  | Node (y, l, r) ->
      if x < y then Node (y, insert x l, r)
      else if x > y then Node (y, l, insert x r)
      else Node (y, l, r)

let construct lst =
  let rec aux acc = function [] -> acc | h :: t -> aux (insert h acc) t in
  aux Empty lst
