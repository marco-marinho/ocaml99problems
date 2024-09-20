let is_prime a =
  let rec aux b n = b * b > n || (n mod b <> 0 && aux (b + 1) n) in
  a > 1 && aux 2 (abs a)
