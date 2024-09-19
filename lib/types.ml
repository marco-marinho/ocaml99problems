type 'a rle = One of 'a | Many of int * 'a

let print_rle = function
  | One a -> Printf.printf "(One : %s) " a
  | Many (c, a) -> Printf.printf "(Many : %s, %d) " a c

type 'a node = OneNode of 'a | ManyNode of 'a node list
