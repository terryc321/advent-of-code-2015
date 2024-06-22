{-# LANGUAGE QuasiQuotes #-}


module Main (main) where

import Text.RawString.QQ
import Test.HUnit
-- import qualified MyLib2 ( byteCount , charCount )
import MyLib2


-- test_000 :: Test
test_000 = TestCase (assertEqual "..." (0 :: Int) (MyLib2.byteCount [r|""|]))

-- test_100 :: Test
test_100 = TestCase (assertEqual "..." (1 :: Int) (MyLib2.byteCount [r|"a"|]))

-- test_200 :: Test 
test_200 = TestCase (assertEqual "..." (2 :: Int) (MyLib2.byteCount [r|"ab"|]))

-- test_300 :: Test
test_300 = TestCase (assertEqual "..." (3 :: Int) (MyLib2.byteCount [r|"abc"|]))

-- test2 = TestCase (do (x,y) <- partA 3
--                      assertEqual "for the first result of partA," 5 x
--                      b <- partB y
--                      assertBool ("(partB " ++ show y ++ ") failed") b)

-- tests :: Test
-- tests = TestList [TestLabel "test1" test1,
--                   TestLabel "test2" test2,
--                   TestLabel "test3" test3 ]


tests :: Test
tests = TestList [ test_000 , test_100 , test_200 ,test_300 ]


main :: IO ()
main = runTestTTAndExit tests

