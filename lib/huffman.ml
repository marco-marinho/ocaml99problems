type node = Leaf of (string * int) | Internal of (node * node * int)

let get_prob = function Leaf (_, x) -> x | Internal (_, _, x) -> x

let sort_node_lst lst =
  List.sort (fun x y -> Int.compare (get_prob x) (get_prob y)) lst

let encode fs =
  let rec gen_tree = function
    | [] -> []
    | [ x ] -> [ x ]
    | a :: b :: t ->
        Internal (a, b, get_prob a + get_prob b) :: t
        |> sort_node_lst |> gen_tree
  in
  let rec gen_code curr = function
    | Leaf (x, _) -> [ (x, curr) ]
    | Internal (x, y, _) -> gen_code (curr ^ "0") x @ gen_code (curr ^ "1") y
  in
  List.map (fun (x, y) -> Leaf (x, y)) fs
  |> sort_node_lst |> gen_tree |> List.hd |> gen_code ""
