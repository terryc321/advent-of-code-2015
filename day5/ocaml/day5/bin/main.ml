
let read_lines filename =
  In_channel.with_open_text filename (fun ic ->
    In_channel.input_lines ic
    |> List.filter (fun s -> String.trim s <> "")
  )

let lines = read_lines "../../input.txt"

(* Sys.chdir("../");; *)
(* Sys.getcwd();; *)

(* It contains at least three vowels (aeiou only), like aei, xazegov, or aeiouaeiouaeiou.
   *)
let has_three_vowels s =
  let count = ref 0 in  
  let foo ch = if (ch = 'a' || ch = 'e' || ch = 'i' || ch = 'o' || ch = 'u')
  then count := (!count) + 1
  else ()
  in String.iter (foo) s ;  (!count) >= 3 

(* It contains at least one letter that appears twice in a row,
   like xx, abcdde (dd), or aabbccdd (aa, bb, cc, or dd).
     *)
let has_one_letter_twice_row s =
  let len = String.length s - 1
  in
  let rec foo i =
    if i < len then if s.[i] == s.[i+1] then true else foo (i + 1 )
    else false 
  in foo 0



(* It does not contain the strings ab, cd, pq, or xy,
   even if they are part of one of the other requirements.
  *)
let has_outlaw_pair s =
  let len = String.length s - 1
  in
  let rec foo i =
    if i < len then if s.[i] == 'a' && s.[i+1] == 'b' then true
    else if s.[i] == 'c' && s.[i+1] == 'd' then true
    else if s.[i] == 'p' && s.[i+1] == 'q' then true
    else if s.[i] == 'x' && s.[i+1] == 'y' then true
    else foo (i + 1 )
    else false 
  in foo 0


let check s =
  let c1 = has_three_vowels s
  and c2 = has_one_letter_twice_row s
  and c3 = not (has_outlaw_pair s)
  in c1 && c2 && c3 

let nice s = (s , check s)

  (*
ugknbfddgicrmopn is nice because it has at least three vowels (u...i...o...), a double letter (...dd...), and none of the disallowed substrings.
aaa is nice because it has at least three vowels and a double letter, even though the letters used by different rules overlap.
jchzalrnumimnmhp is naughty because it has no double letter.
haegwjzuvuyypxyu is naughty because it contains the string xy.
dvszwmarrgswjxmb is naughty because it contains only one vowel.
    *)

let test1 = List.map nice ["ugknbfddgicrmopn" ; "aaa" ; "jchzalrnumimnmhp" ;
  "haegwjzuvuyypxyu" ; "dvszwmarrgswjxmb"]

let part1 = List.filter (fun sv -> let (s,v) = sv in v = true ) (List.map nice lines)


let () = print_endline "Hello, World!"

(* lets compute ! *)
let _ = Printf.printf "part 1 has %d nice strings " (List.length part1)
