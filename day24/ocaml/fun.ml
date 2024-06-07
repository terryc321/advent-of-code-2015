

(* C-x C-e --- eval expression *)

let puz =  [1 ;3 ;5 ;11; 13; 17 ;19 ;23 ;29; 31; 41; 43; 47; 53; 59; 61 ;67 ;71 ;73 ;79; 83; 89; 97; 101; 103; 107; 109; 113 ] ;;

(*
three records
   { nos  : [1,3,5]}
   { len  : 3}
   { sum  : 9}
   { prod : 15 }
place item of puz into one of three groups
   then when no more puz
   if sums are all same then found a solution - print solution
   otherwise ignore return unit ()   
*)

type puz_info =
  {
    nos : int list ;
    len : int ;
    sum : int ;
    prod : int 
  };;

let a = { nos = [1; 2 ; 3] ; len = 3 ; sum = 2 ; prod = 3 } ;;

let b = 321 ;;

(* val len : 'a list -> int ;; *)
let rec len xs =
  if xs = [] then 0
  else (1 + (len (List.tl xs))) ;;

len ['a';'b';'c'];;



(* as is an ocaml reserved word  -- ?? *)
let rec lup x a b c =
  if x = [] then ()
  else begin
       let _ = loopa x a b c ;
       let _ = loopb x a b c ;
       let _ = loopc x a b c ;
           ()
       end 
and
  loopa x a b c =  let v = List.hd x ;
                    let a2 = { nos = v :: a.nos ; len = a.len + 1 ; sum = a.sum + v ; prod = a.prod * v }  in lup (List.tl x) a2 b c 
and
  loopb x a b c =  let v = List.hd x ;
                    let b2 = { nos = v :: b.nos ; len = b.len + 1 ; sum = b.sum + v ; prod = b.prod * v }  in lup (List.tl x) a b2 c 
and
  loopc x a b c =  let v = List.hd x ;
                      let c2 = { nos = v :: c.nos ; len = c.len + 1 ; sum = c.sum + v ; prod = c.prod * v }  in lup (List.tl x) a b c2 ;;
                        

















  
