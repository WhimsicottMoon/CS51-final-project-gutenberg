(*PART 1: Extract information from files*)

let all_word_lists = ref [] (* is a string list list with inner lists as books split into words*)
let file_of_files = "masterfile.txt" (* has addresses of every book to run this on *)

(* for each book, reads in character by character and splits into
   new word when hits space, otherwise adds to current word*)
let make_string (s : string) : string list =
  let next_file_channel = open_in s in
  let word_list = ref [] in
  let next_word = ref "" in
  try
  while true; do
    let next_char = input_char next_file_channel in
    if next_char = ' ' || next_char = '\n' then (word_list := !next_word :: !word_list; next_word := "")
    else next_word := !next_word ^ (Char.escaped next_char)
    done; !next_word :: !word_list;
   with End_of_file -> close_in next_file_channel; !next_word :: !word_list ;;

let take_input =
  (*goes through the master file and gives the address of each book to make_string*)
  let ic = open_in file_of_files in
  try
    while true; do
      let next_file = input_line ic in
      all_word_lists := (make_string next_file) :: !all_word_lists
    done
  with End_of_file -> close_in ic ;;

(* To Do make design better *)

(*PART 2: Parse strings*)

let punctuation = ['.'; '!'; '"'; ':'; ';'; ','; '-'; '\n'] ;; (*TO DO: more punctuation*)

let process_book (lst : string list) : string list =
  (*handles a single string*)
  let process (str : string) : string =
    let lowercase_str = String.lowercase_ascii str in
    List.fold_right (fun c s -> String.concat  "" (String.split_on_char c s))
                    punctuation lowercase_str in
  (*processes entire list*)
  List.fold_right (fun s l -> (process s) :: l) lst [] ;;

let count_freqs (lst : string list) : (string, int) Hashtbl.t =
  let frequencies = Hashtbl.create 3 in
  let add (s : string) : unit =
    if Hashtbl.mem frequencies s then Hashtbl.replace frequencies s (succ (Hashtbl.find frequencies s))
    else Hashtbl.add frequencies s 1 in
  List.iter add lst; Hashtbl.remove frequencies ""; frequencies ;;

let to_rank_list (tbl : (string, int) Hashtbl.t) : (string * int * int) list =
  let compare_word_freq (_, f1 : string * int) (_, f2 : string * int) : int =
    ~-(compare f1 f2) in
  List.mapi (fun i (w,f) -> w, f, i+1 ) (List.sort compare_word_freq (Hashtbl.fold (fun w f acc -> (w, f) :: acc) tbl [])) ;;

let process_all (master_lst : string list list) : (string, int) Hashtbl.t list =
  List.map (fun book -> count_freqs (process_book book)) master_lst ;;

let display =
  let nice_lst = List.map to_rank_list (process_all (!all_word_lists)) in
  List.iter (List.iter (fun (w, f, r) -> Printf.printf "%d: %s  %d\n" r w f)) nice_lst ;;

(*RUN EVERYTHING HERE*)
let () =
  take_input ; display ;;
