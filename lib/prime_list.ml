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
