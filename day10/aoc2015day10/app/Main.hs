module Main (main) where

import Lib

{--
define functions in Main.hs 

For example:

1 becomes 11 (1 copy of digit 1).
11 becomes 21 (2 copies of digit 1).
21 becomes 1211 (one 2 followed by one 1).
1211 becomes 111221 (one 1, one 2, and two 1s).
111221 becomes 312211 (three 1s, two 2s, and one 1).


input = 1113222113

count 1's until no more 1's - then say N `1`s

count number of repeating characters 

"" -> ?

--}

sameR :: Char -> [Char] -> Int -> (Char, Int , [Char])
sameR ch [] n = (ch , n ,  []) -- end 
sameR ch (a : t ) n = if ch == a then sameR ch t (n + 1) -- advance
                      else (ch , n , a : t ) -- no advance


same [] = []
same (h : t) = let (_ , n , rest) = sameR h t 1
                   in (show n) ++ [h] ++ (same rest)
                      

{--
repeated application of function 
--}
rep :: String -> (String -> String) -> Int -> String
rep x f 0 = x
rep x f n = rep (f x) f (n - 1)

input :: String 
input = show 1113222113 :: String

-- do it 40 times
part1 :: String
part1 = rep input (same) 40

part1Length :: Int
part1Length = length part1


-- do it 50 times
part2 :: String
part2 = rep input (same) 50

part2Length :: Int
part2Length = length part2

main :: IO ()
main = do  putStrLn "hello"
           putStrLn $ "part1 length is " ++ show (part1Length)
           putStrLn $ "part2 length is " ++ show (part2Length)
