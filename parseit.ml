(*Just the portion that parses the input strings*)

let original_text = "He Saw H;e was sAw.\n" ;;
let split_text = ["He"; "Saw,"; "H;e"; "was\na"; "sAw"; ".\n"] ;;

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

(*prints the list of stuffs*)
List.iter (fun s -> Printf.printf "%s\n" s) (process_book split_text) ;;