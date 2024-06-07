

--

inp = [ 1,2,3,4,5,7,8,9,10,11 ] 

puz =  [1 , 3 , 5  ,11 , 13,  17,  19,  23 , 29 , 31 , 41 , 43,
       47  ,53,  59,  61,  67,  71,  73 , 79  ,83,  89 , 97 , 101,
       103 , 107,  109,  113 ]


--- generates all combinations of 11 down to 1 
-- foo [] a b c = [(a,b,c)]
-- foo (h:t) a b c = foo1 ++ foo2 ++ foo3  where
--                      foo1 = foo t (h : a) b c
--                      foo2 = foo t a (h : b) c
--                      foo3 = foo t a b (h : c)
min3 a b c = foldr min a [b,c]

--- all same if no value differs from sa 
--- (filter False (nao (= sa) [sa,sb,sc])) = [] 
same3 sa sb sc = if sa == sb then
                    if sb == sc then True
                    else False
                 else False


--- a : list ints
--- sa : sum of those ints
--- la : length of a
--- pa : product of ints multiplied together 
---- similar for b sb lb pb
---- similiar for c sc lc pc
foo [] a b c sa sb sc la lb lc pa pb pc =
  if (same3 sa sb sc) then
     [(a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc)]
  else []
  
foo (h:t) a b c sa sb sc la lb lc pa pb pc =
  foo1 ++ foo2 ++ foo3  where
    foo1 = bar t (h : a) b c (sa + h) sb sc (la + 1) lb lc (pa * h) pb pc
    foo2 = bar t a (h : b) c sa (sb + h) sc la (lb + 1) lc pa (pb * h) pc
    foo3 = bar t a b (h : c) sa sb (sc + h) la lb (lc + 1) pa pb (pc * h)

--- bar sits between invocations of foo , cuts off search space 
bar [] a b c sa sb sc la lb lc pa pb pc =
    foo [] a b c sa sb sc la lb lc pa pb pc

bar (h : t) a b c sa sb sc la lb lc pa pb pc =
  if (min3 la lb lc) > 6 then []
  else foo (h : t) a b c sa sb sc la lb lc pa pb pc
 
           
demo = foo inp [] [] [] 0 0 0 0 0 0 1 1 1
puzzle = foo puz [] [] [] 0 0 0 0 0 0 1 1 1

--- print each entry of puzzle
--- poke puzzle
poke [] = putStrLn ""
poke (h : t) = do putStrLn (show h)
                  poke t

--- take h t
bests [] (a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc)  = [(a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc) ]
bests [(a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc) ] p = (a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc)  : (bests [] p)
bests ((a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc) : tl) (a',b',c',sa',sb',sc',la',lb',lc',pa',pb',pc') =
     if better (a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc) (a',b',c',sa',sb',sc',la',lb',lc',pa',pb',pc')
     then
       (a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc) :
       (bests tl (a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc))
     else (bests tl (a',b',c',sa',sb',sc',la',lb',lc',pa',pb',pc'))
     
---
low3 (l,p,n) (l',p',n') = if l <= l' then
                               if p <= p' then (l,p,n)
                               else (l',p',n')
                          else (l',p',n')
  
lessThan7 (l ,p ,n) = l < 7

filter7h [] xs = xs
filter7h ( (a,b,c) : t) xs = if a < 7 then filter7h t ((a,b,c) : xs) else filter7h t xs
filter7 ys = filter7h ys []

sort3 xs = foldr low3 (head xs) xs

--- which is best between these two things produced 
best (a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc) (a',b',c',sa',sb',sc',la',lb',lc',pa',pb',pc') =  
  let (_,_,win) = sort3 [(la,pa,1),(lb,pb,1),(lc,pc,1),(la',pa',2),(lb',pb',2),(lc',pc',2)]  in
    if win == 1 then (a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc)
    else (a',b',c',sa',sb',sc',la',lb',lc',pa',pb',pc')
 
  -- filter (\x -> lessThan7) [(la,pa,1),(lb,pb,1),(lc,pc,1),(la',pa',2),(lb',pb',2),(lc',pc',2)]

--- A better-than B   : true or false
better (a,b,c,sa,sb,sc,la,lb,lc,pa,pb,pc) (a',b',c',sa',sb',sc',la',lb',lc',pa',pb',pc') =  
 let (_,_,win) = sort3 [(la,pa,1),(lb,pb,1),(lc,pc,1),(la',pa',2),(lb',pb',2),(lc',pc',2)]  in
    if win == 1 then True
    else False

    

first x = head x
second x = head (tail x)
third x = head (tail (tail x))

  
  
--- try again 
deep1 = poke (bests (tail puzzle) (head puzzle))

                  

--- load file into ghci
--- :r reload
--- > demo
--- > puzzle

main = deep1








  

  


