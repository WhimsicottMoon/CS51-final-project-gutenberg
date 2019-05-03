(*Just the portion that parses the input strings*)

let text = "He Saw H;e was sAw.\n" ;;

let punctuation = ['.'; '!'; '"'; ':'; ';'; ','; '-'; '\n'] ;; (*TO DO: more punctuation*)

(*deletes all punctuation by splitting on a character and concatenating once more
for each character in punctuation*)
let strip_punctuation (str : string) : string =
  List.fold_right (fun c s -> String.concat  "" (String.split_on_char c s)) punctuation str;;

let process (s : string) : string list = 
  String.split_on_char ' ' (String.lowercase_ascii (strip_punctuation s)) ;;

(*let count (lst : string list) : (string, int) Hashtbl.t =
  let frequencies = Hashtbl.create 3 in
  List.iter (fun s -> if Hashtbl.mem frequencies s then Hashtbl.add frequencies 1 else Hashtbl.find) *)

List.iter (fun s -> Printf.printf "%s\n" s) (process text) ;;