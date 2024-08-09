
--- import package give it shorthand and only import specific routines required , quite smart
import System.Environment as SE ( getArgs )
import Prelude as P 


{-
^ north
v south
> east
< west 

how many houses get more than one present

-}

-- is this not fold ?

process2 x y [] ys = (x,y) : ys
process2 x y ('^' : xs) ys = process2 x (y - 1) xs ((x,y) : ys)
process2 x y ('v' : xs) ys = process2 x (y + 1) xs ((x,y) : ys)
process2 x y ('<' : xs) ys = process2 (x - 1) y xs ((x,y) : ys)
process2 x y ('>' : xs) ys = process2 (x + 1) y xs ((x,y) : ys)

-- arbitrary start location 1,1 can be anywhere 0,0  -1,-1
-- only match on location and count number times we hit a location more than once
process xs = process2 x0 y0 xs ys
 where x0 = 1
       y0 = 1
       ys = []
       

member x [] = False
member x (y:ys) = if x == y then True else member x ys

--
count2 [] ys n = n 
count2 (x:xs) ys n = if member x ys
                      then count2 xs ys n
                      else count2 xs (x : ys) (n + 1)

count xs = count2 unseen seen tot
 where tot = 0
       seen = []
       unseen = xs

{-
Part two  different puzzle now two santas moving both start at same start square

works for two things but in general moving towards needing a state transition
not sure if we need to store A location (ax,ay) and B location ?

if we mess up maths it means a random house or two will get more than one present
but that house had a present anyway
only interested in a house with atleast one present 

or if independent could split input to those that A uses , those that B uses
then run process with A , process with B , append together , count number 

-}


{-
process2a ax ay bx by [] ys = (ax,ay) : ((bx,by) : ys)
process2a ax ay bx by ('^' : xs) ys = process2b ax (ay - 1) bx by xs ((ax,ay) : ys)
process2a ax ay bx by ('v' : xs) ys = process2b ax (ay + 1) bx by xs ((ax,ay) : ys)
process2a ax ay bx by ('<' : xs) ys = process2b (ax - 1) y  bx by xs ((ax,ay) : ys)
process2a ax ay bx by ('>' : xs) ys = process2b (ax + 1) y  bx by xs ((ax,ay) : ys)

process2b ax ay bx by [] ys = (ax,ay) : ((bx,by) : ys)
process2b ax ay bx by ('^' : xs) ys = process2a ax ay bx (by - 1) xs ((bx,by) : ys)
process2b ax ay bx by ('v' : xs) ys = process2a ax ay bx (by + 1) xs ((bx,by) : ys)
process2b ax ay bx by ('<' : xs) ys = process2a ax ay (bx - 1) by xs ((bx,by) : ys)
process2b ax ay bx by ('>' : xs) ys = process2a ax ay (bx + 1) by xs ((bx,by) : ys)
-}

everyOther :: [a] -> [a]
everyOther [] = []
everyOther (x : []) = [x]
everyOther (x : (_ : xs)) = x : (everyOther xs)

santa1 :: [a] -> [a]
santa1 xs = everyOther xs

santa2 :: [a] -> [a]
santa2 xs = everyOther (tail xs)

{- 
bothSantas xs = count (a ++ b) where
      a = process (santa1 xs)
      b = process (santa2 xs)
-}

----------------------------------------------------------------------------------
{- 
santa [] y = y 
santa (x:xs) y = 
-}

-----------------------------------------------------------------------------------    

main :: IO ()
main = do putStrLn "Please provide input filename "
          putStrLn ""
          args <- SE.getArgs
          let arg1 = if P.length args < 1 then "./input" else args !! 0
          putStrLn ("argument 1 is [ " ++ (P.show arg1) ++ "]")
          file <-readFile arg1
          -- putStrLn file
          let content = process file
          -- putStr "result = "
          -- putStrLn (show result)
          let resultCount = count content
          putStr "result Count = "
          putStrLn (show resultCount)

          -- 
          let santa1content = santa1 content
          let santa2content = santa2 content

          --putStrLn ("santa1 = " ++ (show santa1content))
          --putStrLn ("santa2 = " ++ (show santa2content))

          
          --putStrLn ("santa1 = " ++ (show (length santa1content)))
          --putStrLn ("santa2 = " ++ (show (length santa2content)))
          --putStrLn ("content = " ++ (show (length content)))

--           putStrLn ("both santas = " ++ (show (bothSantas content)))
    
          -- putStrLn ("santa1 = " ++ (show santa1content))
          let both = (count santa1content) + (count santa2content)
          putStrLn ("Part Two - both is  "++ (P.show both))
                    
{- 
          let result = P.sum (P.map wrapping (P.map tri (P.lines file)))
          putStrLn ("Part One - the amount of wrapping paper is  "++ (P.show result))
          let result2 = P.sum (P.map wrapping2 (P.map tri (P.lines file)))
          putStrLn ("Part Two - the amount of wrapping paper is  "++ (P.show result2))
-}
          putStrLn "done"
{- 
          let len = slen file
          putStrLn ("the file is " ++ (show len) ++ " characters long")
          let sol1 = part1 file
          putStrLn ("solution part1 is [ " ++ (show sol1) ++ " ] ")          
          let sol2 = part2 file
          putStrLn ("solution part2 is [ " ++ (show sol2) ++ " ] ")          
-}
          
          
