
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Text.Read as TR 
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map


-- | Represents a coordinate on the 2D grid.
data Point = Point 
  { x :: Int
  , y :: Int
  } deriving (Eq, Ord, Show, Read)

-- | Optional: Make it look nicer in GHCi if you prefer "2,3" over "Point {x = 2, y = 3}"
-- instance Show Point where
--     show (Point a b) = "(" ++ show a ++ "," ++ show b ++ ")"

-- | A grid type alias for clarity. 
-- Map stores the number of presents delivered to that coordinate.
type Grid = Map Point Int

-- | Initialize an empty grid (0 presents everywhere, but we only store visited spots)
emptyGrid :: Grid
emptyGrid = Map.empty

-- | Deliver a present at a specific point.


{--

on final iteration - we must not forget that we ended up somewhere at that last location also
needs to be added to the number of presents delivered map 
up ^ case
down v case
left < case
right > case
--}
iter :: T.Text -> Int -> Int -> Int -> Int -> Grid -> (Grid,Int,Int) 
iter input n len x y grid =
  if n >= len then
    let v = case Map.lookup (Point x y) grid of
          Just n ->  n + 1 
          Nothing -> 1
    in 
      let gfinal = Map.insert (Point x y) v grid
      in (gfinal , n , len) 
  else
    let v = case Map.lookup (Point x y) grid of
          Just n ->  n + 1 
          Nothing -> 1
    in 
    let ch = T.index input n in
    case ch of
      '^' -> iter input (n + 1) len x (y + 1) (Map.insert (Point x y) v grid)
      'v' -> iter input (n + 1) len x (y - 1) (Map.insert (Point x y) v grid)
      '<' -> iter input (n + 1) len (x - 1) y (Map.insert (Point x y) v grid)
      '>' -> iter input (n + 1) len (x + 1) y (Map.insert (Point x y) v grid)
      _ -> (grid,n,len)
      
{-- 
-- need to iterate over all T.pack "^" and
-- grid size == number elements in grid
-- initial grid has point 0 0 already with a present from santa

ip : initial point
ig : initial grid with present at 0,0
len : length of T.Text input
c0 : 0  initial index iterate through len
(g,i,l) = (g=grid i=finish index  l=length == len

--}
solvePart1 :: T.Text -> Int
solvePart1 input =
   let (x0, y0) = (0, 0)
       len = T.length input
       c0 = 0 
   in        
       let ig = Map.insert (Point x0 y0) 0 emptyGrid
       in let (g, i, l) = iter input c0 len x0 y0 ig 
          in
            if i == l
            then Map.size g -- if we finished all input the return size of Map
            else 0 -- otherwise somethign went wrogn just return nonsense


{--

on final iteration - we must not forget that we ended up somewhere at that last location also
needs to be added to the number of presents delivered map 
up ^ case
down v case
left < case
right > case
move santa two string indexes each time - skipping over robo santas instructions
similarly robo santa 
--}
santa2 :: T.Text -> Int -> Int -> Int -> Int -> Grid -> (Grid,Int,Int) 
santa2 input n len x y grid =
  if n >= len then
    let v = case Map.lookup (Point x y) grid of
          Just n ->  n + 1 
          Nothing -> 1
    in 
      let gfinal = Map.insert (Point x y) v grid
      in (gfinal , n , len) 
  else
    let v = case Map.lookup (Point x y) grid of
          Just n ->  n + 1 
          Nothing -> 1
    in 
    let ch = T.index input n in
    case ch of
      '^' -> santa2 input (n + 2) len x (y + 1) (Map.insert (Point x y) v grid)
      'v' -> santa2 input (n + 2) len x (y - 1) (Map.insert (Point x y) v grid)
      '<' -> santa2 input (n + 2) len (x - 1) y (Map.insert (Point x y) v grid)
      '>' -> santa2 input (n + 2) len (x + 1) y (Map.insert (Point x y) v grid)
      _ -> (grid,n,len)


      
{-- 
-- need to iterate over all T.pack "^" and
-- grid size == number elements in grid
-- initial grid has point 0 0 already with a present from santa

ip : initial point
ig : initial grid with present at 0,0
len : length of T.Text input
c0 : 0  initial index iterate through len
(g,i,l) = (g=grid i=finish index  l=length == len

santa delivers all his presents first
robo santa delivers all his presents second
simply count how many 
--}
solvePart2 :: T.Text -> Int
solvePart2 input =
   let (x0, y0) = (0, 0)
       len = T.length input
       c0 = 0 
   in        
       let ig = Map.insert (Point x0 y0) 0 emptyGrid
       in
         let (gsanta, _, _) = santa2 input c0 len x0 y0 ig
         in let (grobo, _, _) = santa2 input (c0 + 1) len x0 y0 gsanta
            in  Map.size grobo


-- input has extra double quotes at start and end of input 
main :: IO ()
main = do  inputWithQuotes <- TIO.readFile "../input"
           let input = T.drop 1 (T.dropEnd 1 inputWithQuotes)
           --TIO.putStrLn $ T.pack "the input was " input
           putStrLn "the input was " 
           TIO.putStrLn input 
           let part1 = solvePart1 input
           putStrLn $ "part 1 solution " ++ show part1 ++ " and goodnight"
           let part2 = solvePart2 input
           putStrLn $ "part 2 solution " ++ show part2 ++ " and goodnight"
        
