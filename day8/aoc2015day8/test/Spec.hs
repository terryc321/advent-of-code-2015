{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit
import Lib(memr , fLen , encode)

import Control.Monad (void) -- Add this import
import System.Exit (exitFailure, exitSuccess)

-- ... (your imports and test definitions) ...

import Lib ()

-- testLetter :: Test
-- testLetter = TestCase $ assertEqual 
--   "Should handle trailing newline"
--   (1,1)
--   (fLen "x" , memr "x") 




  

testLetter :: Test
testLetter = let s = "\"x\""
             in TestCase $ assertEqual 
                "Should handle trailing newline"
                (3,1)
                (fLen s , memr s) 

testEmpty :: Test
testEmpty = let s = "\"\""
             in TestCase $ assertEqual 
                "empty string is worth 2 in the bush"
                (2,0)
                (fLen s , memr s) 


{-- 
For example:
"" encodes to "\"\"", an increase from 2 characters to 6.
"abc" encodes to "\"abc\"", an increase from 5 characters to 9.
"aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase from 10 characters to 16.
"\x27" encodes to "\"\\x27\"", an increase from 6 characters to 11.

using list representation of string makes it +1 easier to figure it out 

--}

testEncode1 :: Test
testEncode1 = let s = [ '"' , '"' ]
                  t = [ '"' , '\\' , '"' , '\\' , '"'  , '"' ]
             in TestCase $ assertEqual 
                "check encode empty string"
                (encode s) 
                t 


testEncode2 :: Test
testEncode2 = let s = [ 'a' , 'b' , 'c' ]
                  t = [ '"' , 'a' , 'b' , 'c' , '"' ]
             in TestCase $ assertEqual 
                "check encode empty string"
                (encode s) 
                t 

-- "aaa\"aaa" encodes to "\"aaa\\\"aaa\"", an increase from 10 characters to 16.
testEncode3 :: Test
testEncode3 = let s = [ '"' , 'a' , 'a' , 'a' , '\\' , '"' , 'a' , 'a' , 'a' , '"' ]
                  t = [ '"' , '\\' , '"' , 'a' , 'a' , 'a' , '\\' , '\\' , '\\' , '"' , 'a' , 'a' , 'a' , '\\' , '"' , '"'  ]
             in TestCase $ assertEqual 
                "check encode empty string"
                (encode s) 
                t 

-- "\x27" encodes to "\"\\x27\"", an increase from 6 characters to 11.
testEncode4 :: Test
testEncode4 = let s = [ '"' , '\\' , 'x' , '2' , '7' , '"'  ]
                  t = [ '"' , '\\' , '"' , '\\' , '\\' , 'x' , '2' , '7' , '\\' , '"' , '"']
             in TestCase $ assertEqual 
                "check encode empty string"
                (encode s) 
                t 



-- --- Test Suite ---

tests :: Test
tests = TestList [
    testLetter,
    testEmpty,
    testEncode1, 
    testEncode2, 
    testEncode3,
    testEncode4 
    -- ,testTurnOff
    -- ,testToggle
    -- ,testWhitespace
    -- ,testNewline
    -- ,testInvalid
    -- testBad
  ]


main :: IO ()
main = do
  counts <- runTestTT tests
  -- Check if there were any failures or errors
  if failures counts > 0 || errors counts > 0
    then do
      putStrLn "Tests FAILED."
      exitFailure  -- This forces the process to exit with code 1
    else do
      putStrLn "All tests passed."
      exitSuccess  -- Explicit success (optional, default is success)

