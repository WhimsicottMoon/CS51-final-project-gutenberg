open Printf
let strings_of_examples = ref []
let file_of_files = "masterfile.txt"

let rec string_list_to_string (lst : string list) (s : string) : string =
  match lst with
  | [] -> s
  | h :: t -> string_list_to_string t (h ^ s) ;;

  let make_string (s : string) : string list =
  let next_file_channel = open_in s in
   let lines = ref [] in
   try
   while true; do
    lines := input_line next_file_channel :: !lines
    done; !lines
   with End_of_file -> close_in next_file_channel; !lines ;;

let () =
let ic = open_in file_of_files in
try
while true; do
  let next_file = input_line ic in
  strings_of_examples := string_list_to_string (make_string next_file) "" :: !strings_of_examples
done
with End_of_file -> close_in ic
 ;;
