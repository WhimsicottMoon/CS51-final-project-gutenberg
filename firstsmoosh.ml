(*PART 1: Extract information from files*)

(* List of string lists, each of which contain all the words in a single book*)
let all_word_lists = ref []

(*A text file with all the file paths to each book's .txt file*)
let file_of_files = "masterfile.txt"

(*List of all the books' file paths*)
let book_files = ref []


(*Read in one char at a time and split at whitespace to form new words,
   operates on the text for a single book *)

let whitespace = [' '; '\n'; '\r']

let make_string (s : string) : string list =
  let next_file_channel = open_in s in
  let word_list = ref [] in
  let next_word = ref "" in
  try
  while true; do
    let next_char = input_char next_file_channel in
    if List.mem next_char whitespace
      then (word_list := !next_word :: !word_list; next_word := "")
    else next_word := !next_word ^ (Char.escaped next_char)
    done; !next_word :: !word_list;
   with End_of_file -> close_in next_file_channel; !next_word :: !word_list ;;

(*Goes through main text file, stores all the book file names,
 and gives the each book's file path to make_string*)
let take_input =
  let ic = open_in file_of_files in
  try
    while true; do
      book_files := !book_files @ [input_line ic]
    done
  with End_of_file -> close_in ic ;
  all_word_lists := List.map make_string !book_files ;;


(*PART 2: Parse strings*)

(*A list of all characters we want to discard, including punctuation, symbols,
 and numbers, which have ASCII codes 33-64, and 91-96 *)
let to_discard = List.init 32 (fun i -> Char.chr (i + 33))
                @ List.init 6 (fun k -> Char.chr (k + 91)) ;;

let process_book (lst : string list) : string list =
  let process_word (str : string) : string =
    let lowercase_str = String.lowercase_ascii str in
    List.fold_right (fun c s -> String.concat  "" (String.split_on_char c s))
                    to_discard lowercase_str in
  List.fold_right (fun s l -> (process_word s) :: l) lst [] ;;

let count_freqs (lst : string list) : (string, int) Hashtbl.t =
  let frequencies = Hashtbl.create 3 in
  let add (s : string) : unit =
    if Hashtbl.mem frequencies s
      then Hashtbl.replace frequencies s (succ (Hashtbl.find frequencies s))
    else Hashtbl.add frequencies s 1 in
  List.iter add lst;
  Hashtbl.remove frequencies "";
  frequencies ;;

let count_all (master_lst : string list list) : (string, int) Hashtbl.t list =
  List.map (fun book -> count_freqs (process_book book)) master_lst ;;

let to_ranked_list (tbl : (string, int) Hashtbl.t) : (string * int * int) list =
  let compare_word_freq (_, f1 : string * int) (_, f2 : string * int) : int =
    ~-(compare f1 f2) in
  List.mapi (fun i (w,f) -> w, f, i+1 )
            (List.sort compare_word_freq
                       (Hashtbl.fold (fun w f acc -> (w, f) :: acc) tbl [])) ;;
let display =
  let nice_lst = List.map to_ranked_list (count_all (!all_word_lists)) in
  List.iter (List.iter (fun (w, f, r) -> Printf.printf "%d: %s  %d\n" r w f))
            nice_lst ;;

(*RUN EVERYTHING HERE*)
let () =
  take_input ; display ;;