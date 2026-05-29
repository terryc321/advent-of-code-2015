module Main (main) where


import Lib (fLen , memr , encode)

-- C-c C-r quick reload
-- C-c C-c build
-- C-c C-k clear screen

main :: IO ()
main = do
       input <- readFile "../input.txt"
       -- part 1 
       let split = lines input
       let fs = sum $ map fLen split
       let ms = sum $ map memr split
       let res = (fs, ms)
       putStrLn $ "day 8 part 1 is " ++ show res
       putStrLn $ "day 8 part 1 = " ++ show (fst res - snd res)
       -- part 2
       let fs2 = fs 
       let ms2 = sum $ map (\s -> length (encode s)) split
       let res2 = ms2 - fs2
       putStrLn $ "day 8 part 2 = " ++ show res2

       
       
       
       
       


      



