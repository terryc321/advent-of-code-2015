module Main where

import HaskellSay (haskellSay)

---import qualified MyLib (someFunc)
import MyLib

-- import MyLib2
--import qualified MyLib2 (someFunc)
-- import qualified MyLib3 (someFunc)


main :: IO ()
main = do haskellSay "Hello, Haskell! You're using a function from another package!"
          putStrLn "Hello, Haskell!"
          putStrLn ""
          putStrLn ""
          MyLib.someFunc
          putStrLn ""
          putStrLn ""
          
          -- MyLib2.someFunc
          -- MyLib3.someFunc


  

