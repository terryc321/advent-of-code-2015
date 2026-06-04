
(* #require "unix";; *)

let time f x =
  let t0 = Unix.gettimeofday () in
  let r = f x in
  let t1 = Unix.gettimeofday () in
  Printf.printf "Elapsed: %.3fs\n" (t1 -. t0);
  r
;;

let md5_hex s i =
  Digest.string (Printf.sprintf "%s%d" s i)
  |> Digest.to_hex

let () = print_endline "Advent code 2015 day 4 - md5 sum puzzle"

let example1 () = md5_hex "abcdef" 609043
let example2 () = md5_hex "pqrstuv" 1048970


let brute str =
  let rec foo i =
    let s = md5_hex str i
    in if (s.[0] = '0' && s.[1] = '0' && s.[2] = '0' && s.[3] = '0' && s.[4] = '0')
    then i
    else foo (i + 1)
  in foo 0

let input = "bgvyzdsv"

let part1 () = brute input

let brute2 str =
  let rec foo i =
    let s = md5_hex str i
    in if (s.[0] = '0' && s.[1] = '0' && s.[2] = '0' && s.[3] = '0' && s.[4] = '0' && s.[5] = '0')
    then i
    else foo (i + 1)
  in foo 0

let part2 () = brute2 input


(* lets compute !  *)
let _ = let p = part1 () in Printf.printf "part1 input was (%s) solution is %d\n" input p 
let _ = let p = part2 () in Printf.printf "part2 input was (%s) solution is %d\n" input p 


  (*
utop[6]> example1 ();;
- : string = "000001dbbfa3a5c83a2d506429c7b00e"
utop[7]> example2 ();;
- : string = "000006136ef2ff3b291c85725f17325c"


    If your secret key is abcdef, the answer is 609043, because the MD5 hash of abcdef609043 starts with five zeroes (000001dbbfa...), and it is the lowest such number to do so.
If your secret key is pqrstuv, the lowest number it combines with to make an MD5 hash starting with five zeroes is 1048970; that is, the MD5 hash of pqrstuv1048970 looks like 000006136ef....
    *)

