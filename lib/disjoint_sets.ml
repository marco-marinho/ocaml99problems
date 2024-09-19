let disjoint n m lst =
  let rec aux acc curr1 curr2 x y = function
    | [] -> if x = 0 && y = 0 then [ acc @ [ curr1; curr2 ] ] else [ acc ]
    | h :: t ->
        if x = 0 && y = 0 then [ acc @ [ curr1; curr2 ] ]
        else if y = 0 then
          aux acc (h :: curr1) curr2 (x - 1) y t @ aux acc curr1 curr2 x y t
        else if x = 0 then
          aux acc curr1 (h :: curr2) x (y - 1) t @ aux acc curr1 curr2 x y t
        else
          aux acc (h :: curr1) curr2 (x - 1) y t
          @ aux acc curr1 (h :: curr2) x (y - 1) t
          @ aux acc curr1 curr2 x y t
  in
  List.filter (fun l -> if l = [] then false else true) (aux [] [] [] n m lst)
