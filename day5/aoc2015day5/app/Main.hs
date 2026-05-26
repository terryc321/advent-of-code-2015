

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Text.Read as TR 
import Data.List (sort)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Data.ByteString (ByteString)
import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 as MD

import qualified Data.ByteString.Lazy.Char8 as BL
-- import qualified Data.ByteString.Lazy as BLStrict -- Optional, if you need strict conversion later

-- | Read a file and return a list of lines as Lazy ByteStrings
readFileLines :: FilePath -> IO [BL.ByteString]
readFileLines path = do
  content <- BL.readFile path
  -- 'lines' splits on '\n' (newline) characters
  return (BL.lines content)

-- day5 read input in as bytestring then we can do some stuff with them 
-- non overlapping pair
-- ab ab
-- ab . ab
--                        ab . . ab
-- given a pair at i      |
-- check forward if 
-- 

solvePart1 s = 123
               
solvePart2 s = 456

main :: IO ()
main = do  input <- readFileLines "../input"
           let part1 = solvePart1 lines  
           putStrLn $ "part 1  = " ++ show part1 ++ " and goodnight"           
           let part2 = solvePart2 lines
           putStrLn $ "part 2 = " ++ show part2 ++ " and goodnight"
        
