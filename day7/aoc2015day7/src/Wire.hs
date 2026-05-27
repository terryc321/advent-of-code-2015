module Wire (
  Source(..),
  Signal(..) ,
  Operation(..),
  Program(..),
  -- Literal,
  -- Wire ,
  -- And ,
  -- Or,
  -- Lshift,
  -- Rshift,
  -- Not,
  -- Direct,
  -- Ufo
  ) where 

import Data.List(isInfixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word16)

-- | Represents a 16-bit signal. 
-- In the puzzle, signals are 16-bit unsigned integers.
type Signal = Word16

-- | Represents the source of a signal: either a literal number or a wire name.
data Source
    = Literal Signal
    | Wire String
    deriving (Show, Eq)

-- | Represents the logic operations defined in the puzzle.
data Operation
    = And Source Source
    | Or Source Source
    | Lshift Source Int
    | Rshift Source Int
    | Not Source
    | Direct Source  -- For instructions like "123 -> x"
    | Ufo [String]  -- the unknown case 
    deriving (Show, Eq)

-- | A program is a map from wire names to their operations.
type Program = Map String Operation


