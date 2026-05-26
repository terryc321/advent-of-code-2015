module Main (main) where

import Debug.Trace  
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Read as TR 
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

-- -- given a line such as "20x3x11" split by "x" then recover 3 values as Int 
-- splitLine :: T.Text -> [T.Text] 
splitLine = T.splitOn (T.pack "x")



bar input = let lines = T.lines input in 
            let firstLine = head lines in 
            let split = splitLine firstLine in
            let cnv = map T.unpack split in 
            let vals = map (\s -> TR.readMaybe s :: Maybe Int ) cnv in
            case vals of
              [Just a , Just b , Just c] -> Just (cnv,vals,present a b c)
              _ -> Nothing  
              
              

main :: IO () 
main = do 
        input <- TIO.readFile "../input.txt"
        let lines = T.lines input 
        let firstLine = head lines 
        TIO.putStrLn firstLine
        let split = splitLine firstLine
        putStrLn "sanity"
         
        
        
        

        -- TIO.putStr $ input 
        -- let split = splitLine firstLine 
        -- let [a,b,c] = split in TIO.putStr $ a 
        --                        TIO.putStr $ b 
        --                        TIO.putStr $ c 
        -- -- putStrLn "this is my text"
        -- -- TIO.putStrLn input 
        -- let triples = map (\s -> map T.unpack (T.splitOn (T.pack "x") s)) lines
        -- -- T.pack   T.unpack 
        -- let part1 = sum (map foo triples)
        -- putStr $ "part1 says " ++ (show part1) ++ " and goodnight"






 





   



