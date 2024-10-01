type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let collect t l =
  let rec aux n acc = function
    | Empty -> acc
    | Node (x, l, r) -> if n = 0 then  x :: acc else aux (n-1) (aux (n-1) acc r) l
in 
aux (l-1) [] t