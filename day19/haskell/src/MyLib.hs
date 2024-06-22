

module MyLib (someFunc ) where

--- get molemolecule and substitutions subs
---import qualified Input (mole,subs)
import Input

{-
subs ... ("e" , "NAl"), ("e" , "OMg")
        A -> B 

mole is string look though to find a substitution

-}


-- Q : how do iterate over string 2 at a time remember what went before and after ?
-- then record it ? 

-- iter2 []    be af =  be
-- iter2 [ _ ] be af =  be
-- iter2 (a1 : a2 : t) be af = 

--- iter b m a s = 
-- ghci> let a = "abcd"
-- ghci> (take 1 a) ++ ['x'] ++ (drop 1 a)
-- "axbcd"
-- ghci> (take 0 a) ++ ['x'] ++ (drop 0 a)
-- "xabcd"
-- ghci> (take 1 a) ++ ['x'] ++ (drop 1 a)
-- "axbcd"
-- ghci> (take 2 a) ++ ['x'] ++ (drop 2 a)
-- "abxcd"
-- ghci> (take 3 a) ++ ['x'] ++ (drop 3 a)
-- "abcxd"
-- ghci> (take 4 a) ++ ['x'] ++ (drop 4 a)
-- "abcdx"
-- ghci> (take 5 a) ++ ['x'] ++ (drop 5 a)
-- "abcdx"
-- ghci> (take 5 a) ++ ['x'] ++ (drop 5 a)
-- "abcdx"
-- ghci> length a
-- 4
-- ghci> (take 3 a) ++ ['x'] ++ (drop 3 a)
-- "abcxd"
-- ghci> (take 4 a) ++ ['x'] ++ (drop 4 a)
-- "abcdx"
-- ghci> let a = mole in (drop 0) ++ (reverse (take 1 (drop 0 a))) ++ (drop 2 a)

-- [ ((take x mole) ++ ['X','Y'] ++ (drop (x + 2) mole))   | x <-  0 .. (length mole) ]

--- list comprehensions ...

--- rather than testing against random when no matches
-- give is some actual data from the puzzle
---"abcdefg"
-- mol = take 10 mole 
mol = mole

a =  [ (take x mol) ++ ['X','Y'] ++ (drop (x + 2) mol) | x <- [0 .. length mol] ]

b =  [ (take x mol) ++ ['X','Y'] ++ (drop (x + 2) mol) | x <- [0 .. ((length mol) - 2) ] ]

--- to took out
--  re replacement
c = let lim = (length mol) - 2
        rg  = [0 .. lim]
        f   = \x -> let to = take 2 (drop x mol)
                        re = ['X','Y']
                        a = take x mol
                        z = drop (x + 2) mol
                    in a ++ re ++ z
       in [ f x | x <- rg ]
          
d = [ y | (x , y )<- subs ,  x == "AI" ]

e = [ y | (x , y )<- subs , x == "Al"]

f = let lim = (length mol) - 2
        rg  = [0 .. lim]
        f   = \x -> let to = take 2 (drop x mol)
                        re = ['X','Y']
                        a = take x mol
                        z = drop (x + 2) mol
                    in a ++ re ++ z
       in [ f x | x <- rg ]


g2c = [ let lim = (length mol) - 2
            rg  = [0 .. lim]
            q   = \x3 y3 z3 -> let to = take 2 (drop x3 mol)
                                in if to == y3 then
                                    let a2 = take x3 mol
                                        z2 = drop (x3 + 2) mol
                                    in a2 ++ z3 ++ z2
                                   else []
         in [ q x po go | x <- rg ]  |  (po , go) <- subs ]

g2b = foldr (++) [] g2c
g2 = filter (\x -> (not (x == ""))) g2b

g1c  = [ let lim = (length mol) - 1
             rg  = [0 .. lim]
             q   = \x3 y3 z3 -> let to = take 1 (drop x3 mol)
                                in if to == y3 then
                                    let a2 = take x3 mol
                                        z2 = drop (x3 + 1) mol
                                    in a2 ++ z3 ++ z2
                                   else []
          in [ q x po go | x <- rg ]  |  (po , go) <- subs ]

g1b = foldr (++) [] g1c
g1 = filter (\x -> (not (x == ""))) g1b


g = g1 ++ g2

remd [] s = s
remd (x:t) s = case member x s of
                True -> remd t s
                False -> remd t (x : s)

member x [] = False
member x (y : t) = if x == y then True
                   else member x t
                   
sol = remd g []

sol_count = length sol

{-
   param1 is built up backwards as move forawrd through list

think of replacement of section of string as



part before   +
           piece to be replaced  +
                                     part after  



-}

-- feeder to fw2 
-- fw s = let (a : b : t) = s
--           in fw2 [] [b] t acc

-- fw2 xs ys zs acc =              


someFunc :: IO ()
someFunc =  do putStr ("There are " ++ show sol_count ++ " different molecules ")
               -- print ["a" , "b" , "c"]
               return ()

               

               


