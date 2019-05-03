(*Just the portion that parses the input strings*)

let text = "He Sa!w H;e was sAw.\n" ;;

let punctuation = ['.'; '!'; '"'; ':'; ';'; ','; '-'; '\n'] ;;

(*deletes all punctuation by splitting on a character and concatenating once more
for each character in punctuation*)
let strip_punctuation (str : string) : string =
  List.fold_right (fun c s -> String.concat  "" (String.split_on_char c s)) punctuation str;;

let process (s : string) : string list = 
  String.split_on_char ' ' (String.lowercase_ascii (strip_punctuation s)) ;;

List.iter (fun s -> Printf.printf "%s\n" s) (process text) ;;