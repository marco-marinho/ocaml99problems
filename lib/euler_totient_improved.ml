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
  aux [] 2 n

let rle lst =
  let rec aux acc count = function
    | [] -> []
    | [ x ] -> (x, count + 1) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux acc (count + 1) t else aux ((a, count + 1) :: acc) 0 t
  in
  aux [] 0 lst

let prime_comp n = rle (prime_factors n)

let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n ->
      let b = pow a (n / 2) in
      b * b * if n mod 2 = 0 then 1 else a

let totient n =
  let rec aux curr = function
    | [] -> curr
    | h :: t ->
        let p, m = h in
        aux ((p - 1) * pow p (m - 1) * curr) t
  in
  aux 1 (prime_comp n)
