{-# LANGUAGE QuasiQuotes #-}


module Main (main) where

import Text.RawString.QQ
import Test.HUnit
-- import qualified MyLib2 ( byteCount , charCount )
import MyLib2


t10 = TestCase (assertEqual "< test_000 >" (2 :: Int) (byteCount [r|""|]))
t12 = TestCase (assertEqual "< test_100 >" (3 :: Int) (byteCount [r|"a"|]))
t13 = TestCase (assertEqual "< test_200 >" (4 :: Int) (byteCount [r|"ab"|]))
t14 = TestCase (assertEqual "< test_300 >" (5 :: Int) (byteCount [r|"abc"|]))

t20 = TestCase (assertEqual "< test_400 >" (0 :: Int) (charCount [r|""|]))
t21 = TestCase (assertEqual "< test_500 >" (1 :: Int) (charCount [r|"a"|]))
t22 = TestCase (assertEqual "< test_600 >" (2 :: Int) (charCount [r|"ab"|]))
t23 = TestCase (assertEqual "< test_700 >" (3 :: Int) (charCount [r|"abc"|]))


tests = TestList [ t10 , t12 , t13 , t14 ]
tests2 = TestList [ t20 , t21 , t22 , t23 ]


main :: IO ()
main = do putStrLn " aha ! will robinson ! "
          runTestTT tests
          runTestTT tests2
          putStrLn " ! we lied ! "
          return ()
          

          
