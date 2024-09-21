let all_true n =
  let rec aux acc = function 0 -> acc | n -> aux (true :: acc) (n - 1) in
  aux [] n

let mark_false n list =
  let rec aux acc curr = function
    | [] -> List.rev acc
    | h :: t ->
        if curr = 0 then aux (false :: acc) (n - 1) t
        else aux (h :: acc) (curr - 1) t
  in
  aux [] n list

let rec to_int_lst acc n curr = function
  | [] -> List.rev acc
  | h :: t ->
      if h = true && curr >= n then to_int_lst (curr :: acc) n (curr + 1) t
      else to_int_lst acc n (curr + 1) t

let list n m =
  let sieve = all_true (m - 2) in
  let rec aux head curr = function
    | [] -> List.rev head
    | h :: t as l -> (
        match h with
        | true -> (
            match mark_false curr l with
            | a :: b -> aux (a :: head) (curr + 1) b
            | [] -> List.rev head)
        | false -> aux (h :: head) (curr + 1) t)
  in
  to_int_lst [] n 2 (aux [] 2 sieve)

let conjecture n =
  let ascending = list 2 n in
  let descending = List.rev ascending in
  let rec aux asc des tar =
    match (asc, des) with
    | [], [] -> (0, 0)
    | [], h :: t -> if h = tar then (0, h) else aux [] t tar
    | h :: t, [] -> if h = tar then (h, 0) else aux t [] tar
    | a :: b, x :: y ->
        if a + x = tar then (a, x)
        else if a + x > tar then aux (a :: b) y tar
        else aux b (x :: y) tar
  in
  aux ascending descending n

let goldbach_list n m =
  let rec aux acc a b =
    if a > b then acc
    else if a mod 2 = 0 then aux ((a, conjecture a) :: acc) (a + 1) b
    else aux acc (a + 1) b
  in
  List.rev (aux [] n m)
