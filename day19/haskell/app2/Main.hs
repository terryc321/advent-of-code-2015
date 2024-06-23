module Main where

import HaskellSay (haskellSay)

-- import MyLib2 

-- import qualified MyLib (someFunc)
import qualified MyLib2 (someFunc)
---import qualified MyLib4 (someFunc)
-- import qualified MyLib3 (someFunc)


main :: IO ()
main = do putStrLn "Hello App2, Haskell!"
          -- MyLib.someFunc
          MyLib2.someFunc
          -- MyLib3.someFunc


  
