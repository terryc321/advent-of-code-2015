module Main (main) where

import Test.HUnit
import qualified MyLib3 ( bc , cc )



test1 = TestCase (assertEqual "some title 0," 0 (1 - 1))
test2 = TestCase (assertEqual "some title 1," 1 (2 - 1))
test3 = TestCase (assertEqual "some title 2," 2 (3 - 1))

-- test2 = TestCase (do (x,y) <- partA 3
--                      assertEqual "for the first result of partA," 5 x
--                      b <- partB y
--                      assertBool ("(partB " ++ show y ++ ") failed") b)

tests = TestList [TestLabel "test1" test1,
                  TestLabel "test2" test2,
                  TestLabel "test2" test3 ]

main :: IO ()
main = do _ <- runTestTT tests
          return ()

-- do putStrLn "Test suite running ..."
