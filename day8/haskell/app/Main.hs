module Main where

import qualified MyLib (someFunc)
import qualified MyLib2 (someFunc)


main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  MyLib.someFunc
  MyLib2.someFunc

  
