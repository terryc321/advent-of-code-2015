module Grid 
 ( -- Types
   createGrid
   ) where


import Data.Array.ST (STUArray, runSTUArray, newArray, writeArray, readArray)
import Data.Array.Unboxed (UArray , (!))
import Control.Monad.ST   (ST, runST)
import Control.Monad      (forM_) -- For looping


-- Creates a 1000x1000 grid, initially all False, and returns it as immutable UArray
createGrid :: UArray (Int, Int) Bool
createGrid = runSTUArray $ do
    grid <- newArray ((0, 0), (999, 999)) False
             :: ST s (STUArray s (Int, Int) Bool)
    
    -- Example mutations (you can remove or expand this)
    -- writeArray grid (0, 0) True
    -- writeArray grid (500, 500) True
    
    return grid

