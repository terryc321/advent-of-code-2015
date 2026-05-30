{-# LANGUAGE FlexibleContexts #-}
module Main(main) where

import Data.Array.ST
import Control.Monad.ST
import Data.Array.IO
import Data.Char (ord, chr)



compute :: Int -> Int
compute n = runST $ do arr <- newArray (-1, 1) n :: ST s (STArray s Int Int)
                       readArray arr 1
                       
{-- 
Passwords must include one increasing straight of at least three letters,
like abc, bcd, cde, and so on, up to xyz.
  They cannot skip letters; abd doesn't count.

straight of three letters 

123
 234
  345
   456
    678
     
--}

-- required :: {-# LANGUAGE FlexibleContexts #-} whatever that was / is 
-- three :: IOArray Int Char -> IO Bool
three arr = do
  loop 1
  where
    loop i
      | i >= 7 = return False
      | otherwise = do
          a <- readArray arr i
          b <- readArray arr (i + 1)
          c <- readArray arr (i + 2)
          let oa = ord a
              ob = ord b
              oc = ord c
          let condition = (oa + 1) == ob && (ob + 1) == oc
          if condition
            then return True
            else loop (i + 1)


-- Passwords may not contain the letters i, o, or l,
-- as these letters can be mistaken for other characters and are therefore confusing.

freeIOL arr = do
  loop 1
  where
    loop i
      | i >= 9 = return True
      | otherwise = do
          a <- readArray arr i
          let condition = a == 'i' || a == 'o' || a == 'l'
          if condition
            then return False
            else loop (i + 1)


{--
-- Passwords must contain at least two different,
-- non-overlapping pairs of letters, like aa, bb, or zz.

1 2 3 4 5 6 7 8 
            * *  last pair 
--}
findPair arr n = do
  loop n
  where
    loop i
      | i > 7 = return False 
      | otherwise = do
          a <- readArray arr i
          b <- readArray arr (i + 1)
          let condition = a == b 
          if condition
            then do other <- findAnotherPair arr (i + 2) a
                    if other then return True
                       else loop (i + 1)
            else loop (i + 1)

            
findAnotherPair arr n c = do
  loop n
  where
    loop i
      | i > 7 = return False 
      | otherwise = do
          a <- readArray arr i
          b <- readArray arr (i + 1)
          let condition = a == b && a /= c
          if condition 
            then return True                 
            else loop (i + 1)


twoDifferent arr = findPair arr 1 

allConditions arr = do
  c1 <- three arr 
  c2 <- freeIOL arr 
  c3 <- twoDifferent arr
  return (c1,c2,c3)
  

showArray inn = do
  a <- readArray inn 1 -- bind each access
  b <- readArray inn 2
  c <- readArray inn 3
  d <- readArray inn 4
  e <- readArray inn 5
  f <- readArray inn 6
  g <- readArray inn 7
  h <- readArray inn 8          
  return (a,b,c,d,e,f,g,h)



next arr = do
  loop 8
  where
    loop i = do ch <- readArray arr i
                if ch == 'z' then do writeArray arr i 'a'
                                     loop (i - 1)
                  else do let ch2 = chr (1 + (ord ch :: Int))
                              ch3 = chr (2 + (ord ch :: Int))
                          let condition = ch2 == 'i' || ch2 == 'o' || ch2 == 'l'
                          writeArray arr i ch2 
                          if condition then do writeArray arr i ch3
                                               return arr
                            else return arr 

brute arr = do
  loop
    where loop = do a <- three arr
                    if not a then do next arr
                                     loop
                      else do b <- freeIOL arr
                              if not b then do next arr
                                               loop
                                else do c <- twoDifferent arr
                                        if not c then do next arr
                                                         loop
                                          else return arr 



{--
--}



{--
  b <- readArray arr (i + 1) 
                       c <- readArray arr (i + 2) 
                       let oa = ord a
                       ob <- ord b
                       oc <- ord c
                       if (oa+1) == ob && (ob + 1) == oc then return True
                         else loop (i + 1)
--}
--  12345678                
-- "hijklmmn"
example1 :: IO (IOArray Int Char)
example1 = do r <- newArray (1,8) 'a'
              writeArray r 1 'h'
              writeArray r 2 'i'
              writeArray r 3 'j'
              writeArray r 4 'k'
              writeArray r 5 'l'
              writeArray r 6 'm'
              writeArray r 7 'm'
              writeArray r 8 'n'            
              return r


---    12345678
-- -- "cqjxjnds"
input :: IO (IOArray Int Char)
input = do r <- newArray (1,8) 'a'
           writeArray r 1 'c'
           writeArray r 2 'q'
           writeArray r 3 'j'
           writeArray r 4 'x'
           writeArray r 5 'j'
           writeArray r 6 'n'
           writeArray r 7 'd'
           writeArray r 8 's'            
           return r
                     


{--
within a do block we can call another IO for side effects inside a do block
if we wish to get  result out of an IO we need to bind using <- inside do block
once we have it bound , we can use it like a normal expression 
--}

main :: IO ()
main = do a <- input
          brute a
          s <- showArray a          
          putStrLn $  "part 1 => " ++ show s
          next a
          brute a
          s <- showArray a
          putStrLn $ "part2 => " ++  show s
          



{---


arr <- newArray (1,8) 'a' :: IO (IOArray Int Char)
a <- readArray arr 1
writeArray arr 1 'z'
b <- readArray arr 1 
print (a,b)
print "hello"
inn <- input  -- bind other IO actions           
a <- readArray inn 1 -- bind each access
b <- readArray inn 2
c <- readArray inn 3
d <- readArray inn 4
e <- readArray inn 5
f <- readArray inn 6
g <- readArray inn 7
h <- readArray inn 8          
print (a,b,c,d,e,f,g,h)
--
ex1 <- example1
prn <- showArray ex1 
allc <- allConditions ex1
print ("example has " , prn , allc)
          
in1 <- input
prn <- showArray in1
allc <- allConditions in1
print ("input has " , prn , allc)

-- in1 <- next input
-- prn <- showArray in1
-- allc <- allConditions in1
-- print ("next input has " , prn , allc)
          

--}
          

          
          

          
          
