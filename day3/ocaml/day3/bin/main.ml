
(*
aoc2015 day3  eggnog dwarfs 
  
  Sys.chdir("../");;
  Sys.getcwd();;

-- depending on where in file hierachy you are ... 
#use "bin/main.ml";;
  
  *)

let content = In_channel.with_open_text "../../input.txt" In_channel.input_all
  
let () = Printf.printf "%s\n" content

let rec iter_string f s i =
  if i < String.length s then (
    f s.[i];
    iter_string f s (i + 1)
  )

(* 1. Create the hash table with an initial capacity *)
let point_map = Hashtbl.create 100

let count_presents () =
  let count = ref 0 in 
  Hashtbl.iter (fun (x,y) value -> count := (!count) + 1) point_map ;
  !count 

(* 2. Define a helper to add points *)
let add_point map x y value =
  Hashtbl.add map (x, y) value

    (* 3. Example usage *)
(* let _ = add_point point_map 1 2 10 ; add_point point_map 3 4 20 *)

(* 4. Retrieve a value *)
let _ = match Hashtbl.find_opt point_map (1, 2) with
| Some v -> Printf.printf "Value: %d\n" v
| None -> Printf.printf "Point not found\n"


let santa = ref (0,0)
let move  = ref 0 

(* assign 0,0 to santa  *)
let santa_reset () = santa := (0,0)

let santa_stamp () =
  move := (!move) + 1 ;
  let (x,y) = !santa in
  (* Printf.printf "santa move(%d) at house (%d,%d)\n" (!move) x y ; *)
  match Hashtbl.find_opt point_map (x, y) with
  | Some v -> Hashtbl.replace point_map (x,y) (v+1) 
  | None -> Hashtbl.replace point_map (x,y) 1  
 
let santa_down () =
  let (x,y) = !santa in
  santa := (x,y-1) ;
  santa_stamp () 

let santa_up () =
  let (x,y) = !santa in
  santa := (x,y+1);
  santa_stamp () 


let santa_left () =
  let (x,y) = !santa in
  santa := (x-1,y);
  santa_stamp () 


let santa_right () =
  let (x,y) = !santa in
  santa := (x+1,y);
  santa_stamp () 


let nop () = ()

(* santa having side effects ?  *)
let process_char c =
  match c with
  | 'v' -> santa_down ()
  | '^' -> santa_up ()
  | '<' -> santa_left ()
  | '>' -> santa_right ()
  | '\n' -> nop () 
  | _ -> invalid_arg (Printf.sprintf "direction not understood: %c" c)

let check s = 
  Hashtbl.clear point_map ; 
  santa_reset () ;
  santa_stamp () ; 
  iter_string (fun c -> process_char c) s 0 ;
  let (x,y) = !santa in
  Printf.printf "There are houses with atleast one present (size of hashtable ) : %d\n" (Hashtbl.length point_map) ;
  Printf.printf "There are houses with atleast one present (iter hashtable ) : %d\n" (count_presents ()) ;  
  Printf.printf "santa ended up at (%d,%d)\n" x y  


(* clear hashtable , set santa to 0 0 , deliver present to 0,0 , each move , deliver another present
how many houses receive atleast one present 
   *)
let part1 () = check content

  (*
> delivers presents to 2 houses: one at the starting location, and one to the east.
^>v< delivers presents to 4 houses in a square, including twice to the house at his starting/ending location.
^v^v^v^v^v delivers a bunch of presents to some very lucky children at only 2 houses.
    *)
    
let example1 () = check ">"
let example2 () = check "^>v<"
let example3 () = check "^v^v^v^v^v"


let sanity () =
  let h = Hashtbl.create 10 in
  Hashtbl.add h (1,2) 3;
  Hashtbl.add h (1,2) 5;
  Hashtbl.add h (1,2) 7;
  Hashtbl.length h 

let sanity2 () =
  let h = Hashtbl.create 10 in
  Hashtbl.replace h (1,2) 3;
  Hashtbl.replace h (1,2) 5;
  Hashtbl.replace h (1,2) 7;
  Hashtbl.length h 


let rec iter_string_skip f s i =
  if i < String.length s then (
    f s.[i];
    iter_string_skip f s (i + 2)
  )


let check2 s = 
  Hashtbl.clear point_map ; 
  santa_reset () ;
  santa_stamp () ; 
  iter_string_skip (fun c -> process_char c) s 0 ;
  santa_reset () ;
  santa_stamp () ; 
  iter_string_skip (fun c -> process_char c) s 1 ;  
  Printf.printf "There are houses with atleast one present (size of hashtable ) : %d\n" (Hashtbl.length point_map) 


let example1b () = check2 ">"
let example2b () = check2 "^>v<"
let example3b () = check2 "^v^v^v^v^v"
let part2 () = check2 content

(* work the problem *)
let _ = part1 ()
let _ = part2 ()

