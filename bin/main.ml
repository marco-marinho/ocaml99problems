open Problems99_lib;;


let test_list = [1; 2; 3; 4; 5; 6]

let _ = print_string "1 - List tail: ";; 
        print_int (Tail_of_a_list.intoption_to_int (Tail_of_a_list.list_tail test_list));; 
        print_string "\n";;

let _ = print_string "2 - List last two: ";;
        let (x, y) = (Last_two_elements.inttupleoption_to_int (Last_two_elements.last_two_elements test_list)) in
        Printf.printf "%d, %d\n%!" x y

let _ = Printf.printf "3 - Nth element: %d \n" (Nth.nth test_list 4)

let _ = print_string "4 - List length: ";; print_int (List_length.lst_length test_list);; print_endline "";;


let _ = print_string "5 - List rev: ";; List.iter (Printf.printf "%d,") (List_rev.list_rev test_list);; print_endline "";;

let test_list = [1;2;3;2;1]
let test_list_false = [5;4;4;2;1]

let _ = Printf.printf "6 - Palindrome: %B\n" (Palindrome.is_palindrome test_list)
let _ = Printf.printf "6 - Palindrome: %B\n" (Palindrome.is_palindrome test_list_false)

let test_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let test_lst = [Types.OneNode "a"; Types.ManyNode [Types.OneNode "b"; Types.ManyNode [Types.OneNode "c" ;Types.OneNode "d"]; Types.OneNode "e"]]

let _ = print_string "7 - Flatten a list: ";; List.iter (print_string) (Flatten_a_list.flatten test_lst);; print_endline "";;

let test = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let _ = print_string "8 - Eliminate duplicate: ";; 
        List.iter print_string (Eliminate_duplicate.eliminate_duplicates test);;
        print_endline "";;

let _ = print_string "Modified run length encoding : ";; 
        List.iter Types.print_rle (Modified_run_length_encoding.encode test_list);;
        print_string "\n";;

let test_list = [
Types.Many (4, "a"); 
Types.One "b"; 
Types.Many (2, "c"); 
Types.Many (2, "a"); 
Types.One "d"; 
Types.Many (4, "e")]

let _ = List.iter (Printf.printf "%s, ") (Decode_run_length_encoded_list.decode test_list)


