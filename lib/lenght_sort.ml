let sort lst =
  let rec aux acc = function
    | f :: s :: t ->
        if List.length f > List.length s then aux [] ((s :: acc) @ (f :: t))
        else aux (f :: acc) (s :: t)
    | h :: [] -> h :: acc
    | [] -> []
  in
  List.rev (aux [] lst)

let rle lst =
  let rec aux acc count = function
    | [] -> []
    | [ x ] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux acc (count + 1) t else aux ((a, count + 1) :: acc) 0 t
  in
  aux [] 0 lst

let rec get_val (key : int) = function
  | [] -> failwith "Key not found"
  | (a, b) :: t -> if key = a then b else get_val key t

let sort_freq lst =
  let lengths = List.map List.length (sort lst) in
  let len_freq = rle lengths in
  let rec aux acc = function
    | a :: b :: t ->
        if get_val (List.length a) len_freq < get_val (List.length b) len_freq
        then aux [] ((b :: acc) @ (a :: t))
        else aux (a :: acc) (b :: t)
    | a :: [] -> a :: acc
    | _ -> acc
  in
  aux [] lst
