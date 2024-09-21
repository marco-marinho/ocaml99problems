let is_prime a =
  let rec aux b n = b * b > n || (n mod b <> 0 && aux (b + 1) n) in
  a > 1 && aux 2 (abs a)

let prime_factors n =
  let rec aux acc m = function
    | 1 | 0 -> acc
    | n ->
        if is_prime m && n mod m = 0 then aux (m :: acc) m (n / m)
        else aux acc (m + 1) n
  in
  List.rev (aux [] 2 n)
