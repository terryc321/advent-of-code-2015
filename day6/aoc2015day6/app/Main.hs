module Main (main) where

import Data.Array.Unboxed (UArray, (!))

import Text.Parsec (eof , parse)
import Text.Parsec.String (parseFromFile)

import Lib (instructionParser, Instruction)
import Grid (createGrid)


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


       

