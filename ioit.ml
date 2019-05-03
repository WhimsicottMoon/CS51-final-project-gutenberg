let all_word_lists = ref [] (* is a string list list with inner lists as books  split into words*)
let file_of_files = "masterfile.txt" (* has addresses of every book to run this on *)

(* for each book, reads in character by character and splits into
   new word when hits space, otherwise adds to current word
 *)
let make_string (s : string) : string list =
  let next_file_channel = open_in s in
  let word_list = ref [] in
  let next_word = ref "" in
  try
  while true; do
    let next_char = input_char next_file_channel in
    if next_char = ' ' then (word_list := !next_word :: !word_list; next_word := "")
    else next_word := !next_word ^ "next_char"
    done; !next_word :: !word_list;
   with End_of_file -> close_in next_file_channel; !next_word :: !word_list ;;

(*goes through the master file and gives the address of each book to make_string*)
let () =
  let ic = open_in file_of_files in
  try
  while true; do
    let next_file = input_line ic in
    all_word_lists := (make_string next_file) :: !all_word_lists
  done
  with End_of_file -> close_in ic
 ;;


(* To Do make design better *)
