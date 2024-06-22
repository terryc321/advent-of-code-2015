

module MyLib (someFunc) where

-- redundant IO already in 
import System.IO
        


{-

tricky to test inside interpreter because
whatever write as string inside interpreter gets
processed and backslashes are removed BEFORE
routines have a chance to do anything



given a string - add 2 for code because of " "  two quotes
count char count
count memory count

\' rep 1 char , 2 mem
\\ rep 1 char , 2 mem
\xXX represents 1 char , 4 mem 
-}

f :: String -> Int -> Int -> (Int,Int)
f [] c m = (c,m)
f ( '\\' : t) c m  = (c,0) -- 
f ( '\\' : '\'' : t) c m  = f t (c + 1) (m + 2)
f ( '\\' : '\\' : t) c m  = f t (c + 1) (m + 2)
f ( '\\' : 'x' : _ : _ : t) c m  = f t (c + 1) (m + 4)
f ( h : t) c m = f t (c + 1) (m + 1)

f2 x = f x 0 0

-- g :: [(Int,Int)]
-- g = map f2 input

-- pr [] 
-- 
-- h = map pr g

process2 :: [String] -> Int -> Int -> (Int,Int)
process2 [] c m = (c,m)
process2 (h:t) c m = let (c2,m2) = f2 h
                         c3 = c2 + c
                         m3 = m2 + m
                     in process2 t c3 m3

process :: [String] -> (Int,Int)                        
process xs = process2 xs 0 0 


split2 :: String -> String -> [String] -> [String]
split2 [] [] s = s
split2 [] w s = w : s 
split2 (h : t) w s = if h == '\n' then  split2 t [] ((reverse w) : s)
                    else split2 t (h : w) s

split :: String -> [String]
split xs = reverse (split2 xs [] [])



--- first stab at read file in haskell , use simpler slup hGetContents
--- then if file is empty it dont throw errors ? no/?
someFunc :: IO ()
someFunc = do fileHandle <- openFile "src/input" ReadMode
              contents <- hGetContents fileHandle
              --putStrLn contents
              let (c,m) = process (split contents) 
                   in do putStrLn (" [ " ++ show c ++ " , " ++ show m ++ " ] ")
                         return ()
              hClose fileHandle
              


              
-- someFunc :: IO ()
-- someFunc = do putStrLn "hello world!"
-- putStr (head (split contents)) -- _ <- process (split contents) 0 0
-- _ <- process (split contents) 0 0
              -- return ()
