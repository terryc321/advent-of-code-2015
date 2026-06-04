(* ai slop
   suggested angstrom
   opam install angstrom 
   *)

open Str
open Fmt
open Angstrom

(* Parser for one or more digits, converted to int *)
let p_int = 
  take_while1 (function '0'..'9' -> true | _ -> false)
  >>= fun s -> return (int_of_string s)

(* Parser for the literal character 'x' *)
let p_x = char 'x'

(* Grammar: int 'x' int 'x' int *)
let p_line = 
  let* x = p_int in
  let* _ = p_x in
  let* y = p_int in
  let* _ = p_x in
  let* z = p_int in
  return (x, y, z)

(* Usage *)
(* let () = *)
(*   let input = "20x3x11" in *)
(*   match parse_string ~consume:All p_line input with *)
(*   | Ok (x, y, z) -> Printf.printf "Parsed: %d %d %d\n" x y z *)
(*   | Error msg -> Printf.eprintf "Failed: %s\n" msg *)

let parse_line line =
  match parse_string ~consume:All p_line line with
  | Ok (x, y, z) -> (x , y , z)
  | Error msg -> (Printf.eprintf "Failed: %s\n" msg ;  (0,0,0))


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


let vals = read_and_parse "../../input.txt"

(* 2*l*w + 2*w*h + 2*h*l.
   l w h
   smallest side lw lh wh 
   *)

let wrap triple =
  let (l,w,h) = triple 
  in let area = 2*l*w + 2*w*h + 2*h*l
  in let sorted = List.sort compare [l*w ; l*h ; w*h]
  in area + List.hd(sorted)

(* Sum a list using fold_right *)
let wrapped = List.map (wrap) vals

let part1 = List.fold_right (fun x acc -> x + acc) wrapped 0

(* smallest perimeter around any one face
   l w h
   l w
   l   h
     w h
   *)
   
let wrap2 triple =
  let (l,w,h) = triple 
  in let perims = List.sort compare [2*(l+w) ; 2*(l+h) ; 2*(w+h) ]
  in let product = l*w*h
  in product + List.hd(perims)

(* Sum a list using fold_right *)
let wrapped2 = List.map (wrap2) vals

let part2 = List.fold_right (fun x acc -> x + acc) wrapped2 0


(* let () = *)
(*   (\* Dump.list automatically handles the list structure *\) *)
(*   (\* Dump triple handles the (int * int * int) structure *\) *)
(*   let p_tuple fmt (x, y, z) = pf fmt "(%d, %d, %d)" x y z in *)
(*   (\* Correct usage: pf stdout format_string arguments *\) *)
(*   pf stdout "%a@." Dump.(list p_tuple) vals *)
  
let () =
  Printf.printf "Part1: %d\n" part1 ;
  Printf.printf "Part2: %d\n" part2


