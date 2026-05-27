module Main (main) where

import Text.Parsec
import Text.Parsec.String
import Lib (Grid3 , createGrid3 )

main = do input <- readFile "../input.txt"
          let split = lines input
          let results = map (\line -> parse (instructionParser <* eof) "line" line) split
          let successes = [ instr | Right instr <- results ]
          putStrLn $ "Parsed " ++ show (length successes) ++ " instructions."
          if not (null successes)
            then do
              putStrLn "First 5:"
              mapM_ print (take 5 successes)
            else
              putStrLn "No instructions parsed."
              
          let finalGrid = createGrid2 successes
          putStrLn $ "Light at (0,0): "     ++ show (finalGrid ! (0, 0))
          putStrLn $ "Light at (500,500): " ++ show (finalGrid ! (500, 500))
          -- Count how many lights are on (Part 1 answer)
          let lightsOn = length [ () | x <- [0..999], y <- [0..999], finalGrid ! (x,y) ]
          putStrLn $ "Lights that are on: " ++ show lightsOn

          let finalGrid3 = createGrid3 successes
          -- Count how many lights are on (Part 2 answer)
          let lightsOn3 = length [ () | x <- [0..999], y <- [0..999], finalGrid3 ! (x,y) ]
          putStrLn $ "Lights that are on: " ++ show lightsOn3

       

