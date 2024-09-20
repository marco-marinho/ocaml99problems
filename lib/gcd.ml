let rec gcd a b =
  match (a, b) with
  | 0, r | r, 0 -> r
  | a, b when a > b -> gcd (a mod b) b
  | a, b when b > a -> gcd a (b mod a)
  | _ -> a
