type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let rec eval a val_a b val_b = function
  | Var x ->
      if x = a then val_a
      else if x = b then val_b
      else failwith "Invalid variable"
  | Not x -> not (eval a val_a b val_b x)
  | And (x, y) -> eval a val_a b val_b x && eval a val_a b val_b y
  | Or (x, y) -> eval a val_a b val_b x || eval a val_a b val_b y

let table a b expr =
  [
    (true, true, eval a true b true expr);
    (true, false, eval a true b false expr);
    (false, true, eval a false b true expr);
    (false, false, eval a false b false expr);
  ]

let rec get_val a = function
  | [] -> failwith "Invalid variable"
  | (n, v) :: t -> if n = a then v else get_val a t

let rec eval_table table = function
  | Var x -> get_val x table
  | Not x -> not (eval_table table x)
  | And (x, y) -> eval_table table x && eval_table table y
  | Or (x, y) -> eval_table table x || eval_table table y

let gen_possibilities lst =
  let rec aux acc = function
    | [] -> [ List.rev acc ]
    | h :: t -> aux ((h, true) :: acc) t @ aux ((h, false) :: acc) t
  in
  aux [] lst

let table_table table expr =
  let possibilities = gen_possibilities table in
  List.map (fun x -> [ (x, eval_table x expr) ]) possibilities
