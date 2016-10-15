(* emulate unix grep in ocaml *)
open Arg
open Unix
open Str
open Buffer
open String
open Bytes
open Char

let walk_directory_tree dir =
  let rec walk acc = function
    | [] -> acc
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
       walk (files @ acc) (dirs @ tail)
  in
  walk [] [dir]

let read_file file_name =
  let input_line_opt ic =
    try Some (input_line ic)
    with End_of_file -> None
  in
  let ic = open_in file_name in
  let rec loop acc =
    match input_line_opt ic with
    | Some line -> loop (line::acc);
    | None -> (List.rev acc);
  in
  loop [];;

let match_lines pattern lines =
  let select = fun line ->
    try
      ignore (Str.search_forward (Str.regexp pattern) line 0); true;
    with Not_found -> false in
  List.filter select lines;;




let is_binary_file file =
  let len = 1024 in
  let ic = open_in file in
  let read_bytes1 = Bytes.create len in
  let read_len =  (input ic read_bytes1 0 100) in
  let read_bytes = Bytes.sub read_bytes1 0 (read_len-1) in
  let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
  let text_list = [7;8;9;10;12;13;27] @ (range 0x20 0x7f) @
                    (range 0x80 0x100)in
  let remove_bytes = Bytes.create (List.length text_list) in
  List.iteri
    (fun i integer ->
      Bytes.set remove_bytes i (Char.chr integer))
    text_list;
  let strip str remove_bytes =
    let n = String.length str in
    let res = String.make n 'c' in
    let rec loop i j =
      if i >= n then String.sub res 0 j
      else
        if String.contains remove_bytes str.[i] then
          loop (succ i) j
        else begin
            res.[j] <- str.[i];
            loop (succ i) (succ j)
          end
    in
    loop 0 0
  in
  let rem_str = strip read_bytes remove_bytes in
  if((String.length rem_str) >0) then
    true
  else
    false;;

let match_file pattern file =
  let lines = read_file file in
  let matched = match_lines pattern lines in
  if (is_binary_file file) then
    begin
      print_string ("binary file " ^ file ^ " matched");
      print_newline ();
      [];
    end
  else
    begin
    List.iter (fun line ->
      begin
      print_string (file ^ ":");
      print_endline line;
      end)
            matched;
    matched;
    end


let rec match_files pattern acc  = function
    | [] -> acc
    | file::files  ->
       match_files pattern (acc @ (match_file pattern file))
                   files

let rec match_file_or_directory pattern recursive acc = function
  | dirf::dirfs ->
     begin
     match (stat dirf).st_kind with
     | S_REG -> (match_file pattern dirf)@acc
     | S_DIR ->
        begin
          match recursive with
          | true ->
             match_files pattern [] (walk_directory_tree dirf)
          | false ->
             match_file_or_directory pattern recursive acc dirfs
        end
     | _ -> match_file_or_directory pattern recursive acc dirfs
     end
  | _ -> acc


let usgmsg = "Usage: grep [OPTION]... PATTERN [FILE]... \n\
              Search for PATTERN in each FILE or standard input.
              PATTERN is, by default, a basic regular expression (BRE).
              Example: grep -i 'hello world' menu.h main.c
              "
let usage () = print_endline usgmsg
let print_usage msg = print_endline msg

let main () =
  let recursive = ref false in
  let anon_args = ref [] in
  let speclist = [("-h", Unit (usage), "print simple tutorial");
                  ("-r", Arg.Set recursive, "recursively matche")]
  in
  let store_args s =
    anon_args := s::!anon_args
  in
  Arg.parse speclist store_args  "";
  anon_args := List.rev !anon_args;
  match !anon_args with
  | a::b -> ignore(match_file_or_directory a !recursive [] b)
  | _ -> print_endline "no input file specified"

let () = main()
