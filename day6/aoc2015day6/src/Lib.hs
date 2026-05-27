module Lib 
 ( -- Types
    Instruction(..)  -- Exports the type and its constructors (TurnOn, etc.)
  , Command(..)
  , Coord(..)
  -- Functions
  , instructionParser
  , turnOffParser
  , turnOnParser
  , parseInput
  , solvePart1
  , solvePart2
  ) where


import Data.Array.IArray (Array, listArray, (!), (!?))
import Data.Maybe (fromMaybe)

import Text.Parsec
import Text.Parsec.String (Parser)

-- 1. Data Types (Generic for many puzzles)
data Command = TurnOn | TurnOff | Toggle deriving (Show, Eq)
data Coord = Coord { x :: Int, y :: Int } deriving (Show, Eq)
data Instruction = Instruction { cmd :: Command, start :: Coord, end :: Coord } deriving (Show, Eq)

-- 2. Primitive Parsers (Reusable across years)
number :: Parser Int
number = read <$> many1 digit

coord :: Parser Coord
coord = do
  xVal <- number
  char ','
  yVal <- number
  return (Coord xVal yVal)

-- 3. Specific Command Parsers
turnOnParser :: Parser Command
turnOnParser = string "turn on" >> return TurnOn

turnOffParser :: Parser Command
turnOffParser = string "turn off" >> return TurnOff

toggleParser :: Parser Command
toggleParser = string "toggle" >> return Toggle

-- 4. Composing the full instruction
instructionParser :: Parser Instruction
instructionParser = do
  c <- try turnOnParser
       <|> try turnOffParser
       <|> try toggleParser
  spaces
  s <- coord
  spaces 
  string "through"
  spaces 
  e <- coord  
  spaces   -- eat newlines 
  return (Instruction c s e)

-- 5. Processing the whole input
parseInput :: String -> [Instruction]
parseInput input = case parse (instructionParser `sepBy` newline) "" input of
  Left err -> error (show err)
  Right instrs -> instrs



-- Type alias for readability
type Grid = Array (Int, Int) Bool

-- Create a grid where every light is False (Off)
initialGrid :: Grid
initialGrid = listArray ((0, 0), (999, 999)) (repeat False)



solvePart1 x = 123
solvePart2 x = 456


