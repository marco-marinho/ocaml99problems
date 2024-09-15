type 'a rle =
  | One of 'a
  | Many of int * 'a

let to_rle (count, value) = 
  match count with 
  | 1 -> One(value)
  | c -> Many((c, value))

let encode lst =
  let rec aux acc count = function
    | [] -> []
    | [x] -> to_rle (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux acc (count+1) t
        else aux ((to_rle (count + 1, a)) :: acc) 0 t
  in
  List.rev (aux [] 0 lst)

let print_rle = function
  | One(a) -> Printf.printf "(One : %s) " a
  | Many(c, a) -> Printf.printf "(Many : %s, %d) " a c

let test_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let _ = List.iter print_rle (encode test_list)
