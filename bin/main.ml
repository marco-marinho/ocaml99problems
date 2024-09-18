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

let test_lst = [Types.OneNode "a"; Types.ManyNode [Types.OneNode "b"; Types.ManyNode [Types.OneNode "c" ;Types.OneNode "d"]; Types.OneNode "e"]]

let _ = print_string "7 - Flatten a list: ";; List.iter (print_string) (Flatten_a_list.flatten test_lst);; print_endline "";;

let test = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let _ = print_string "8 - Eliminate duplicate: ";; 
        List.iter print_string (Eliminate_duplicate.eliminate_duplicates test);;
        print_endline "";;

let test_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
let res = (Pack_consecutive_duplicate.pack_consecutive_duplicates test_list)

let _ = print_string "9 - Pack consecutive duplicates: ";; 
        List.iter (List.iter print_string) res;; 
        print_endline "";;

let print_tuple = function
  | (value, count ) -> (Printf.printf "(%s, %d) " value count)

let test_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let _ = print_string "10 - Run length encoding:";; 
        List.iter print_tuple (Run_length_encoding.encode test_list);; 
        print_endline "";;

let _ = print_string "11 - Modified run length encoding : ";; 
        List.iter Types.print_rle (Modified_run_length_encoding.encode test_list);;
        print_string "\n";;

let test_list = [
Types.Many (4, "a"); 
Types.One "b"; 
Types.Many (2, "c"); 
Types.Many (2, "a"); 
Types.One "d"; 
Types.Many (4, "e")]

let _ = print_string "12 - Decode run length encoder: ";; 
        List.iter (Printf.printf "%s, ") (Decode_run_length_encoded_list.decode test_list);;
        print_endline "";;


let print_tuple = function
  | (value, count ) -> (Printf.printf "(%s, %d) " value count)

let test_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let _ = print_string "13 - Run lenght encoding direct: ";;
        List.iter print_tuple (Run_length_encoding_direct.encode test_list);;
        print_endline "";;

let test_lst = ["a"; "b"; "c"; "d"]

let _ = print_string "14 - Duplicate list elements: ";
      List.iter (Printf.printf "%s, ") (Duplicate_elements_of_a_list.duplicate test_lst);
      print_endline "";;

let _ = print_string "15 - Replicate elements n times: ";
      List.iter (Printf.printf "%s, ") (Replicate_elements_n_times.replicate test_lst 4);
      print_endline "";;

let _ = print_string "16 - Drop every nth element: ";
      List.iter (Printf.printf "%s, ") (Drop_every_n_element.drop test_lst 2);
      print_endline "";;

let test_list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]

let _ = print_string "17 - Split list into two: ";
      let (x, y) = (Split_list.split_list test_list 5) in
      print_endline "";
      List.iter (Printf.printf "%s, ") x;
      print_endline "";
      List.iter (Printf.printf "%s, ") y;
      print_endline "";;

let _ = print_string "18 - List Slice: ";
      List.iter (Printf.printf "%s, ") (Slice_of_a_list.slice 3 7 test_list);
      print_endline "";;

let _ = print_string "19 - Rotate List : ";
      List.iter (Printf.printf "%s, ") (Rotate_list_n.rotate 4 test_list);
      print_endline "";;

let _ = print_string "20 - Remove nth element: ";
      List.iter (Printf.printf "%s, ") (Remove_n_element.remove 4 test_list);
      print_endline "";;

let _ = print_string "21 - Insert element: ";
      List.iter (Printf.printf "%s, ") (Insert_element.insert "rato" 4 test_list);
      print_endline "";;

let _ = print_string "22 - Integer range: ";
      print_endline "";
      List.iter (Printf.printf "%d, ") (Interger_in_range.get_range 6 10);
      print_endline "";
      List.iter (Printf.printf "%d, ") (Interger_in_range.get_range 10 6);
      print_endline "";;

Random.self_init ();;
  
let _ = print_string "23 - Random elements: ";
      List.iter (Printf.printf "%s, ") (Random_elements.get 3 test_list);
      print_endline "";;


let _ = print_string "24 - Draw N: ";
      List.iter (Printf.printf "%d, ") (Draw_n.draw 3 10);
      print_endline "";;
      

let _ = print_string "25 - Random list perm: ";
      List.iter (Printf.printf "%s, ") (Random_perm.perm test_list);
      print_endline "";;
      
