module Lib 
 ( -- Types
    Instruction(..)  -- Exports the type and its constructors (TurnOn, etc.)
  , Command(..)
  , Coord(..)
  , Grid(..)
  , Grid3(..)
  -- Functions
  , instructionParser
  , turnOffParser
  , turnOnParser
  , parseInput
  , createGrid
  , createGrid2
  , createGrid3  
  , (!)
  , solvePart1
  , solvePart2
  , sepBy
  , newline
  , spaces
  ) where



-- -------------------------------------------------------------------------------------
-- parsing imports parsec 
import Text.Parsec 
import Text.Parsec.String 

-- -------------------------------------------------------------------------------------
-- grid imports 
import Data.Array.ST (STUArray, runSTUArray, newArray, writeArray, readArray)
import Data.Array.Unboxed (UArray , (!))
import Control.Monad.ST   (ST, runST)
import Control.Monad      (forM_) -- For looping


-- -------------------------------------------------------------------------------------
-- no more imports past this line 


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


-- -------------------------------------------------------------------------------------

solvePart1 x = 123
solvePart2 x = 456

-- -------------------------------------------------------------------------------------
-- Creates a 1000x1000 grid, initially all False, and returns it as immutable UArray
createGrid :: UArray (Int, Int) Bool
createGrid = runSTUArray $ do
    grid <- newArray ((0, 0), (999, 999)) False
             :: ST s (STUArray s (Int, Int) Bool)
    
    -- Example mutations (you can remove or expand this)
    -- writeArray grid (0, 0) True
    -- writeArray grid (500, 500) True    
    return grid

-- a useful abbreviation -- a grid is an ummutable two d array returning a boolean 
type Grid = UArray (Int, Int) Bool



-- this grid takes a list of instructions and produces a grid  
createGrid2 :: [Instruction] -> Grid
createGrid2 instructions = runSTUArray $ do
    grid <- newArray ((0, 0), (999, 999)) False
             :: ST s (STUArray s (Int, Int) Bool)

    forM_ instructions $ \instr -> do
        let (Coord x1 y1) = start instr
            (Coord x2 y2) = end instr

        forM_ [x1 .. x2] $ \x ->
          forM_ [y1 .. y2] $ \y -> do
            current <- readArray grid (x, y)
            let newVal = case cmd instr of
                    TurnOn  -> True
                    TurnOff -> False
                    Toggle  -> not current
            writeArray grid (x, y) newVal

    return grid


-- part2 this is a new grid with integer values now instead of boolean
type Grid3 = UArray (Int, Int) Int

createGrid3 :: [Instruction] -> Grid3
createGrid3 instructions = runSTUArray $ do
    grid <- newArray ((0, 0), (999, 999)) 0
             :: ST s (STUArray s (Int, Int) Int)

    forM_ instructions $ \instr -> do
        let (Coord x1 y1) = start instr
            (Coord x2 y2) = end instr

        forM_ [x1 .. x2] $ \x ->
          forM_ [y1 .. y2] $ \y -> do
            current <- readArray grid (x, y)
            let newVal = case cmd instr of
                    TurnOn  -> current + 1 
                    TurnOff -> max 0 (current - 1)
                    Toggle  -> current + 2 
            writeArray grid (x, y) newVal

    return grid




