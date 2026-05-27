{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Test.HUnit

import Text.Parsec 
import Text.Parsec.String 

import qualified Data.Text as T
import Control.Monad (void) -- Add this import
import System.Exit (exitFailure, exitSuccess)

-- ... (your imports and test definitions) ...


import Lib (Instruction(..), Command(..), Coord(..), instructionParser , sepBy , spaces , newline)

-- ... (keep your parseInstruction and test cases exactly as before) ...


-- Helper to run the parser on a String
-- Returns Either String Instruction
parseInstruction :: String -> Either String Instruction
parseInstruction input = 
  case parse (instructionParser <* eof) "input" input of
    Left err  -> Left (show err)
    Right val -> Right val

-- --- Test Cases ---

-- Test "turn on"
testTurnOn :: Test
testTurnOn = TestCase $ assertEqual 
  "Should parse 'turn on' command"
  (Right (Instruction TurnOn (Coord 0 0) (Coord 2 2)))
  (parseInstruction "turn on 0,0 through 2,2")

-- Test "turn off"
testTurnOff :: Test
testTurnOff = TestCase $ assertEqual 
  "Should parse 'turn off' command"
  (Right (Instruction TurnOff (Coord 10 10) (Coord 20 20)))
  (parseInstruction "turn off 10,10 through 20,20")

-- Test "toggle"
testToggle :: Test
testToggle = TestCase $ assertEqual 
  "Should parse 'toggle' command"
  (Right (Instruction Toggle (Coord 5 5) (Coord 5 5)))
  (parseInstruction "toggle 5,5 through 5,5")

-- Test Whitespace resilience
testWhitespace :: Test
testWhitespace = TestCase $ assertEqual 
  "Should handle extra spaces"
  (Right (Instruction TurnOn (Coord 1 1) (Coord 2 2)))
  (parseInstruction "turn on   1,1   through   2,2")

-- Test Trailing newline (common in file inputs)
testNewline :: Test
testNewline = TestCase $ assertEqual 
  "Should handle trailing newline"
  (Right (Instruction TurnOff (Coord 0 0) (Coord 0 0)))
  (parseInstruction "turn off 0,0 through 0,0\n")

-- Test Invalid Input
testInvalid :: Test
testInvalid = TestCase $ assertBool 
  "Should fail on invalid command"
  (isLeft (parseInstruction "blink on 0,0 through 2,2"))
  where
    isLeft (Left _) = True
    isLeft (Right _) = False

testBad :: Test
testBad = TestCase $ assertBool
  "this should fail and test should report Failed  "
  False 


-- --- Test Suite ---

tests :: Test
tests = TestList [
    testTurnOn
    ,testTurnOff
    ,testToggle
    ,testWhitespace
    ,testNewline
    ,testInvalid
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

