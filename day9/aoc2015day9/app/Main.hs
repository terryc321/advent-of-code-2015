module Main (main) where


import Lib (Trip(..)
           , theTrips
           , allPlaces
           , distances
           , look
           , permutation
           , development
          )

import Control.Monad (mapM_)
import qualified Data.Map as M
import qualified Data.List as L

-- C-c C-r quick reload
-- C-c C-c build
-- C-c C-k clear screen
--input <- readFile "../example.txt"


main :: IO ()
main = do input <- readFile "../input.txt"       
          mapM_ (\s -> putStrLn $ show s) (development input)
       -- let me = ["a","b","c"]
       -- putStrLn $ "fx = " ++ show (fx me)
       -- putStrLn $ "gx = " ++ show (foldr (++) [] (gx (fx me)))
       -- putStrLn $ "gx2 = " ++ show (foldr (++) [] (gx (foldr (++) [] (gx (fx me)))))
       
       -- putStrLn $ "gx2 = " ++ show (gx (gx (fx places)))
       -- putStrLn $ "gx3 = " ++ show (gx (gx (fx places)))
       -- putStrLn $ "gx4 = " ++ show (gx (gx (gx (fx places))))
       
       
       -- part 1 
       -- input <- readFile "../input.txt"
       -- let fs = sum $ map fLen split
       -- let ms = sum $ map memr split
       -- let res = (fs, ms)
       -- putStrLn $ "day 8 part 1 is " ++ show res
       -- putStrLn $ "day 8 part 1 = " ++ show (fst res - snd res)
       -- -- part 2
       -- let fs2 = fs 
       -- let ms2 = sum $ map (\s -> length (encode s)) split
       -- let res2 = ms2 - fs2
       -- putStrLn $ "day 8 part 2 = " ++ show res2

       
       
       
       
       


      



