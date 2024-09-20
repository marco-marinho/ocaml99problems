let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let coprime a b = gcd a b = 1

let totient n =
  let rec aux acc a b =
    if a >= b then acc
    else if coprime a b then aux (acc + 1) (a + 1) b
    else aux acc (a + 1) b
  in
  if n = 1 then 1 else aux 0 1 n
