(*Just the portion that parses the input strings*)

let original_text = "He Saw H;e was sAw.\n" ;;
let split_text = ["He"; "Saw"; "H;e"; "was"; "sAw"; ".\n"] ;;

let punctuation = ['.'; '!'; '"'; ':'; ';'; ','; '-'; '\n'] ;; (*TO DO: more punctuation*)

(*deletes all punctuation/other whitespace by splitting on specificed characters
and concatenating once more*)
let strip_punctuation (str : string) : string =
  List.fold_right (fun c s -> String.concat  "" (String.split_on_char c s)) punctuation str;;

let process (s : string) : string = 
  String.lowercase_ascii (strip_punctuation s) ;;

let rec process_all (lst : string list) : string list =
  match lst with
  | [] -> []
  | hd :: tl -> (process hd) :: process_all tl ;; (*TO DO: can this be made nicer with a fold?*)

let count (lst : string list) : (string, int) Hashtbl.t =
  let frequencies = Hashtbl.create 3 in
  let add (s : string) : unit =
    if Hashtbl.mem frequencies s then Hashtbl.replace frequencies s (succ (Hashtbl.find frequencies s))
    else Hashtbl.add frequencies s 1 in
  List.iter add lst; frequencies ;;

List.iter (fun s -> Printf.printf "%s\n" s) (process_all split_text) ;;