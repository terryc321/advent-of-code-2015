
import System.Environment as SE
import Prelude as P 

input = 3842356

{-
each line should form pattern 
    integer  x integer x integer
otherwise line is ignored 

-}

-- tokenising
-- parsing 

--- extract a value from string - and compute int at same time 
tok2 :: String -> Int -> (Int , String)
tok2 [] n = (n , [])
tok2 ('0' : xs) n  = tok2 xs (10 * n + 0) 
tok2 ('1' : xs) n  = tok2 xs (10 * n + 1)  
tok2 ('2' : xs) n  = tok2 xs (10 * n + 2) 
tok2 ('3' : xs) n  = tok2 xs (10 * n + 3)  
tok2 ('4' : xs) n  = tok2 xs (10 * n + 4) 
tok2 ('5' : xs) n  = tok2 xs (10 * n + 5) 
tok2 ('6' : xs) n  = tok2 xs (10 * n + 6) 
tok2 ('7' : xs) n  = tok2 xs (10 * n + 7) 
tok2 ('8' : xs) n  = tok2 xs (10 * n + 8) 
tok2 ('9' : xs) n  = tok2 xs (10 * n + 9) 
tok2 xs n =  (n,xs)


tok :: String -> (Int , String)
tok xs = tok2 xs 0

--- if given a good XXXxYYYxZZZ return list of [x,y,z] 
tri xs = let (n1,xs2) = tok xs in
          let (n2,xs3) = tok (P.tail xs2) in
              let (n3,_) = tok (P.tail xs3) in
                (n1,n2,n3)

-- smallest area 
smallestArea :: (Int,Int,Int) -> Int
smallestArea (l,w,h) = P.minimum [l*w, l*h, w*h]

surfaceArea :: (Int,Int,Int) -> Int
surfaceArea (l,w,h) = 2*l*w + 2*w*h + 2*h*l

wrapping :: (Int,Int,Int) -> Int
wrapping (l,w,h) = surfaceArea(l,w,h) + smallestArea(l,w,h)                

smallestPerimeter :: (Int,Int,Int) -> Int
smallestPerimeter (l,w,h) = P.minimum [2*(l+w), 2*(l+h), 2*(w+h)]

cubic :: (Int,Int,Int) -> Int
cubic (l,w,h) = l * w * h

wrapping2 :: (Int,Int,Int) -> Int
wrapping2 (l,w,h) = (smallestPerimeter (l,w,h) ) + (cubic (l,w,h))


main :: IO ()
main = do putStrLn "Please provide input filename "
          putStrLn ""
          args <- SE.getArgs
          let arg1 = if P.length args < 1 then "./input" else args !! 0
          putStrLn ("argument 1 is [ " ++ (P.show arg1) ++ "]")
          file <-readFile arg1
--           putStrLn file
          let result = P.sum (P.map wrapping (P.map tri (P.lines file)))
          putStrLn ("Part One - the amount of wrapping paper is  "++ (P.show result))
          let result2 = P.sum (P.map wrapping2 (P.map tri (P.lines file)))
          putStrLn ("Part Two - the amount of wrapping paper is  "++ (P.show result2))
          putStrLn "done"
{- 
          let len = slen file
          putStrLn ("the file is " ++ (show len) ++ " characters long")
          let sol1 = part1 file
          putStrLn ("solution part1 is [ " ++ (show sol1) ++ " ] ")          
          let sol2 = part2 file
          putStrLn ("solution part2 is [ " ++ (show sol2) ++ " ] ")          
-}
          
          
