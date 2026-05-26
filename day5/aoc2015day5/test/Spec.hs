
import Test.HUnit
import Lib

tests :: Test
tests = TestList
  [ "aaa is nice"          ~: isNice "aaa"          ~?= True
  , "jchzalrnumimnmhp"     ~: isNice "jchzalrnumimnmhp" ~?= False
  , "haegwjzuvuyypxyu"     ~: isNice "haegwjzuvuyypxyu" ~?= False
  , "ugknbfddgicrmopn"     ~: isNice "ugknbfddgicrmopn" ~?= True
  ]

main :: IO Counts
main = runTestTT tests

