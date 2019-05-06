open Plplot

(*PART 1: Extract information from files*)

(* List of string lists, each of which contain all the words in a single book *)
let all_word_lists = ref []

(* A text file with all the file paths to each book's .txt file *)
let file_of_files = "Nonfiction.txt"

(* List of all the books' file paths *)
let book_files = ref []

(* Read in one char at a time and split at whitespace to form new words,
   operates on the text for a single book
 *)
let make_string (s : string) : string list =
  let whitespace = [' '; '\n'; '\r'] in
  let next_file_channel = open_in s in
  let word_list = ref [] in
  let next_word = ref "" in
  try
  while true; do
    let next_char = input_char next_file_channel in
    if List.mem next_char whitespace
      then (word_list := !next_word :: !word_list; next_word := "")
    else next_word := !next_word ^ (Char.escaped next_char)
    done;
    !next_word :: !word_list;
   with End_of_file -> close_in next_file_channel; !next_word :: !word_list ;;

(* Goes through main text file, stores all the book file names,
  and gives the each book's file path to make_string
 *)
let take_input =
  let ic = open_in file_of_files in
  try
    while true; do
      book_files := !book_files @ [input_line ic]
    done
  with End_of_file -> close_in ic ;
  all_word_lists := List.map make_string !book_files ;;


(*PART 2: Parse strings*)

(* A list of all characters we want to discard, including punctuation, symbols,
   and numbers, which have ASCII codes 33-64, and 91-96
 *)
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

let rank_book (tbl : (string, int) Hashtbl.t) : (string * int * int) list =
  let compare_word_freq (_, f1 : string * int) (_, f2 : string * int) : int =
    ~-(compare f1 f2) in
  List.mapi (fun i (w,f) -> w, f, i+1 )
            (List.sort compare_word_freq
                       (Hashtbl.fold (fun w f acc -> (w, f) :: acc) tbl [])) ;;

let rank_all (all_words : string list list) : (string * int * int) list list =
  List.map rank_book (count_all all_words) ;;

(* Part 3: Export and Graph Results *)

(*adijfkas;d*)
let nice_lst = rank_all !all_word_lists

(* Writes word frequency rankings to different files for each book *)
let export_results =
  let export_book (i : int) (file_name : string) : unit =
    let oc = open_out ("Ranking of " ^ file_name) in
    List.iter (fun (w, f, r) -> Printf.fprintf oc "%d: %s  %d\n" r w f)
              (List.nth nice_lst i);
    close_out oc in
  List.iteri export_book !book_files ;;

(* Set up plot boundaries, orientation, and labels; initialize *)
let xmin = 0.0 in
let xmax = 5.0 in
let ymin = 0.0 in
let ymax = 5.0 in
plsdiori 1.0;
plinit ();
plenv xmin xmax ymin ymax 0 0 ;
pllab "log of rank" "log of frequency" ("Zipf's Law for " ^ (List.hd (String.split_on_char '.' file_of_files)));

(* Plot each point as log of rank and frequency with plstring, and save
   the last point in order to connect the dots. Change color with each book
 *)
let oldx = ref 0. in
let oldy = ref 0. in
let colorer = ref 3 in
let plotter ((w, f, r) : (string * int * int)) : unit =
  plcol0 !colorer;
  let rlog = log10(float_of_int r) in
  let flog = log10(float_of_int f) in
  plstring [|rlog|] [|flog|] "#(728)";
  (if !oldy <> 0. then pljoin !oldx !oldy rlog flog;
  oldx := rlog;
  oldy := flog;) in
  List.iter (fun lst -> List.iter plotter lst; colorer := !colorer + 1;) nice_lst ;;

  (* Plplot has a lot of options for the pllegend function, so these are all
     variables that set up the legend followed by running pllegend
   *)
let l = List.length !book_files in
let text_colors = Array.init l (fun x -> x + 3) in
let text = Array.init l (fun x -> List.hd (String.split_on_char '.'
                                          (List.nth !book_files x))) in
let line_colors = Array.init l (fun x -> x + 3) in
let line_styles = Array.make l 1 in
let line_widths = Array.make l 1.0 in
let symbol_colors = Array.init l (fun x -> x + 3) in
let opt_array = Array.make l [PL_LEGEND_LINE; PL_LEGEND_SYMBOL] in
let symbol_scales = Array.make l 1.0 in
let symbol_numbers = Array.make l 3 in
let symbols = Array.make l "#(728)" in
pllegend [PL_LEGEND_BOUNDING_BOX] []
         0.0 0.0 0.1 15
         1 1 0 0
         opt_array
         1.0 1.0 2.0
         1.0 text_colors text
         [||] [||] [||] [||]
         line_colors line_styles line_widths
         symbol_colors symbol_scales symbol_numbers symbols ;;
