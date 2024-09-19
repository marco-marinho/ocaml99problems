let to_rle (count, value) =
  match count with 1 -> Types.One value | c -> Types.Many (c, value)

let encode lst =
  let rec aux acc count = function
    | [] -> []
    | [ x ] -> to_rle (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux acc (count + 1) t
        else aux (to_rle (count + 1, a) :: acc) 0 t
  in
  List.rev (aux [] 0 lst)
