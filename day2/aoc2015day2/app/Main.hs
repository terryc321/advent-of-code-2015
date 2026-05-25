module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
import Data.List (sort)

--- Maybe a = Nothing | Just a 
--- focus on solving - 

present :: Int -> Int -> Int -> Int 
foo :: [String] -> Int 


present x y z = let wrap  = 2*x*y + 2*x*z + 2*y*z
                in let sorted = sort [x,y,z] 
                   in let (a : b : _) = sorted 
                      in wrap + (a * b) 

-- import Lib
-- (a,b,c) 
-- Either Left or Right ?
--
foo [a,b,c] = let aa = read a in 
              let bb = read b in 
              let cc = read c in 
              case (aa,bb,cc) of
               (Just an , Just bn , Just cn) -> present an bn cn 
               _ -> -9999999999999999




-- -- using Data.Text , what dependency do we need to add ?
-- --- in strict mode 
-- main :: IO () 
-- main = do 
--         input <- TIO.readFile "../input.txt"
--         -- putStrLn "this is my text"
--         -- TIO.putStrLn input 
--         let lines = T.lines input 
--         -- using partial application - last entry comes from the line element 
--         let triples = map (\s -> map T.unpack (T.splitOn (T.pack "x") s)) lines
--         -- T.pack   T.unpack 
--         let part1 = sum (map foo triples)
--         putStr $ "part1 says " ++ (show part1) ++ " and goodnight"


main :: IO () 
main = do 
       putStr $ "hello world"





 





   


