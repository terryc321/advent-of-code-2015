(* get current working directory in ocaml *)
let cwd = Sys.getcwd();;

(* get first character of a string  
   String.get "asdf" 0;;
   not entirely accurate at all depending on the string encoding may not even be decodable
   at all - see perl unicode 17 compliance
   besides mostly our strings in advent of code are simple ascii characters 
 *)

(* how do i explore String module
   we can see String module using #show String;;  *)

(* how do i load current file into emacs ocaml buffer from M-x run-ocaml
   #use "main.ml" *)

(* this should be in a library file somewhere as all advent of code utilities will use it *)
let read_file filename = 
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done; !lines
  with End_of_file ->
    close_in chan;
    List.rev !lines ;;

(* how do i print a small int to console ? *)
(* how do i print a big int to console ? *)

let asdf_len = String.length "asdf"

(* caution in is a reserved word in ocaml *)

(* if we iterate over all characters in string
   if character is ')' has type char 
   ')' ;; 
   - : char = ')'   
 *)

(* manually iterating over a string with index and conditional if this then this

   elev : elevator procedure santa 
   f : is the floor santa find himself on - starts at 0
   santa1 : takes a string and returns an int 
 *)

let santa1 (s : string) : int =
  let len = String.length s
  in let rec elev i f = if i < len then (* i index into string ok *)
                         let ch = String.get s i
                         in if ch = '(' then elev (i + 1) (f + 1)
                            else if ch = ')' then elev (i + 1) (f - 1)
                            else (* bad char *)
                              elev (i + 1) f
                        else (* no more chars *)
                          f
     in let index = 0
        and floor = 0
        in elev index floor 


(* for exposition of reader - part 1 is solved using santa 1   *)
let part1 = santa1

(* we can define a variable with name of a type - and ocaml does not get confused  *)
let char = 3 

(*    in this instance string s can contain garbage ,
      for each ( ) we increment a character count
      when floor is less than 0 we inform what character we are at
 *)      
let santa2 (s : string) : int =
  let len = String.length s
  in let rec elev2 (i : int) (f : int) (c : int) =
       if f < 0 then c
       else 
         if i < len then (* i index into string ok *)
           let ch = String.get s i
           in if ch = '(' then elev2 (i + 1) (f + 1) (c + 1)
              else if ch = ')' then elev2 (i + 1) (f - 1) (c + 1)
              else (* bad char *)
                elev2 (i + 1) f c
         else (* no more chars - no valid position found *)
           -1
     in let index = 0
        and floor = 0
        and char = 0 
        in elev2 index floor char 


(* for exposition of reader - part 2 is solved using santa 2   *)
let part2 = santa2

(* is explicit type declaration helpful - how does one write generic code ?  *)


(* toplevel anonymous definitions use unit ()  *)
let () = let lines = read_file "../../../input"
         in let line = List.hd lines
            in print_string "the line has " ;
               print_int (String.length line) ;
               print_string " characters." ;
               print_newline () ;
               print_string "santa1 ends up on floor " ;
               print_int (santa1 line) ;
               print_newline () ;
               print_string "santa2 enters basement at character " ;
               print_int (santa2 line) ;
               print_newline () ;
               
               




