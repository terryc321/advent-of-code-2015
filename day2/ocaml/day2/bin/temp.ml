(* let () = print_endline "Hello, World!";;
   
   {??} C-x C-e	Evaluate the current phrase (most common).  Sends the code ending at the cursor to utop.
   {X} C-x C-r	Evaluate the selected region. 
   {X}   C-c C-b	Evaluate the entire buffer. 
   {X}C-c C-z	Switch focus to the utop REPL buffer. 
   {X}C-c C-s	Start a new utop process (if not running).
   *)

open Str

let parse_line line =
  (* Split the line by 'x' *)
  let parts = split (regexp "x") line in
  (* Ensure there are exactly 3 parts and convert to int *)
  match parts with
  | [a; b; c] -> (int_of_string a, int_of_string b, int_of_string c)
  | _ -> failwith "Invalid line format: expected 20x3x11"

let read_and_parse filename =
  let ic = open_in filename in
  let rec loop acc =
    try
      let line = input_line ic in
      let (x, y, z) = parse_line line in
      loop ((x, y, z) :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  loop []   


let vals = read_and_parse "../../../input.txt"

(* >#use "main.ml";;
   >vals
val vals : (int * int * int) list =
  [(20, 3, 11); (15, 27, 5); (6, 29, 7); (30, 15, 9); (19, 29, 21);
   (10, 4, 15); (1, 26, 4); (1, 5, 18); (10, 15, 23); (10, 14, 20);
   (3, 5, 18); (29, 23, 30); (7, 4, 10); (22, 24, 29); (30, 1, 2);
   (19, 2, 5); (11, 9, 22); (23, 15, 10); (11, 11, 10); (30, 28, 5);
   (22, 5, 4); (6, 26, 20); (16, 12, 30); (10, 20, 5); (25, 14, 24);
   (16, 17, 22); (11, 28, 26); (1, 11, 10); (1, 24, 15); (13, 17, 21);
   (30, 3, 13); (20, 25, 17); (22, 12, 5); (22, 20, 24); (9, 2, 14);
   (6, 18, 8); (27, 28, 24); (11, 17, 1); (1, 4, 12); (5, 20, 13);
   (24, 23, 23); (22, 1, 25); (18, 19, 5); (5, 23, 13); (8, 16, 4);
   (20, 21, 9); (1, 7, 11); (8, 30, 17); (3, 30, 9); (6, 16, 18);
   (22, 25, 27); (9, 20, 26); (16, 21, 23); (5, 24, 17); (15, 17, 15);
   (26, 15, 10); (22, 16, 3); (20, 24, 24); (8, 18, 10); (23, 19, 16);
   (1, 21, 24); (23, 23, 9); (14, 20, 6); (25, 5, 5); (16, 3, 1);
   (29, 29, 20); (11, 4, 26); (10, 23, 24); (29, 25, 16); (27, 27, 22);
   (9, 7, 22); (6, 21, 18); (25, 11, 19); (14, 13, 3); (15, 28, ...); ...]
   
   #require "str"
 > #show Str 
     sig
    type regexp
    val regexp : string -> regexp
    val regexp_case_fold : string -> regexp
    val quote : string -> string
    val regexp_string : string -> regexp
    val regexp_string_case_fold : string -> regexp
    val string_match : regexp -> string -> int -> bool
    val search_forward : regexp -> string -> int -> int
    val search_backward : regexp -> string -> int -> int
    val string_partial_match : regexp -> string -> int -> bool
    val matched_string : string -> string
    val match_beginning : unit -> int
    val match_end : unit -> int
    val matched_group : int -> string -> string
    val group_beginning : int -> int
    val group_end : int -> int
    val global_replace : regexp -> string -> string -> string
    val replace_first : regexp -> string -> string -> string
    val global_substitute : regexp -> (string -> string) -> string -> string
    val substitute_first : regexp -> (string -> string) -> string -> string
    val replace_matched : string -> string -> string
    val split : regexp -> string -> string list
    val bounded_split : regexp -> string -> int -> string list
    val split_delim : regexp -> string -> string list
    val bounded_split_delim : regexp -> string -> int -> string list
    type split_result = Text of string | Delim of string
    val full_split : regexp -> string -> split_result list
    val bounded_full_split : regexp -> string -> int -> split_result list
    val string_before : string -> int -> string
    val string_after : string -> int -> string
    val first_chars : string -> int -> string
    val last_chars : string -> int -> string

   *)
