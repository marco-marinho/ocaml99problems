type 'a binary_tree = Empty | Node of 'a * 'a binary_tree * 'a binary_tree

let rec cbal_tree n =
  if n = 0 then Empty
  else
    let m = n - 1 in
    Node ("x", cbal_tree ((m / 2) + (m mod 2)), cbal_tree (m / 2))
