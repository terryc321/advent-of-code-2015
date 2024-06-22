
module Main (main) where

import System.Exit

main :: IO ()
main = do putStrLn " *** W A R N I N G *** "
          putStrLn " *** W A R N I N G *** "
          putStrLn " *** W A R N I N G *** "   
          putStrLn " "
          putStrLn " cabal test -- is unsafe false negatives "
          putStrLn " "          
          putStrLn " *** Please run safer alternatives ***"
          putStrLn " "
          putStrLn " "          
          putStrLn " cabal run app-test"
          putStrLn " cabal run app2-test"
          putStrLn " "          
          putStrLn " *** W A R N I N G *** "
          putStrLn " *** W A R N I N G *** "
          putStrLn " *** W A R N I N G *** "   
          exitFailure
          
