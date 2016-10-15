(* emulate unix grep in ocaml *)
open Arg
open Unix
open Str

let walk_directory_tree dir pattern =
  let select str = Str.string_match (Str.regexp pattern) str 0 in
  let rec walk res = function
    | [] -> res
    | dir::tail ->
       let contents = Array.to_list (Sys.readdir dir) in
       let contents = List.map (Filename.concat dir) contents in
       let files, dirs =
         List.fold_left (fun (files, dirs) f ->
             match (stat f).st_kind with
             | S_REG -> (f::files, dirs)
             | S_DIR -> (files, f::dirs)
             | _ -> (files, dirs)

           ) ([],[]) contents
       in
       let matched = List.filter select files in
       walk (matched @ res) (dirs @ tail)
  in
  walk [] [dir]

let match_file pattern file_name =
  let matched_lines = ref [] in
  let ic = open_in file_name in

  begin
  try
    while true
    do
      let line = input_line ic in
      if (Str.string_match (Str.regexp pattern) line 0)
      then
         matched_lines := line::!matched_lines
    done;
  with End_of_file -> close_in ic
  end;
  List.rev !matched_lines;;




let usgmsg = "Usage: grep [OPTION]... PATTERN [FILE]... \n\
              Search for PATTERN in each FILE or standard input.
              PATTERN is, by default, a basic regular expression (BRE).
              Example: grep -i 'hello world' menu.h main.c
              "
let usage () = print_endline usgmsg
let print_usage msg = print_endline msg

let speclist = [("-h", Unit (usage), "print simple tutorial")]

let main () =
  let anon_args = ref [] in
  let store_args s =
    anon_args := s::!anon_args
  in
  Arg.parse speclist store_args  "";
  match !anon_args with
  | [a; b] -> List.iter print_endline (match_file b a)
  | _ -> print_endline ""

let () = main()
