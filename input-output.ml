open Printf

let file_of_files = "fileset"

let () =
let ic = open_in file_of_files in
try
while true do
  let next_file = input_line ic in
  let next_file_channel = open_in next_file in
done
with End_of_file -> close_in file_of_files
| ;;
