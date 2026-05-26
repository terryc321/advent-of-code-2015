
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


threeVowelsRec :: Int -> [Char] -> Bool
threeVowelsRec _ [] = False
threeVowelsRec n (h : t ) =
  (n >= 3) || (case h of
         _ | h `elem` "aeiou" -> threeVowelsRec (n + 1) t
         _ | otherwise -> threeVowelsRec n t)

threeVowels :: [Char] -> Bool
threeVowels = threeVowelsRec 0

twiceInRow :: [Char] -> Bool
twiceInRow [] = False
twiceInRow [_] = False
twiceInRow (x : y : t ) = (x == y) || twiceInRow (y : t)


avoidPair :: [Char] -> Bool
avoidPair [_] = True
avoidPair (x : y : t) =
    case (x, y) of
        ('a', 'b') -> False
        ('c', 'd') -> False
        ('p', 'q') -> False
        ('x', 'y') -> False
        _ -> avoidPair (y : t)

nice :: [Char] -> Bool
nice s = let a = threeVowels s
             b = twiceInRow s
             c = avoidPair s
         in a && b && c


solvePart1 xs = length $ filter nice xs

solvePart2 xs = 456

main :: IO ()
main = do  input <- readFile "../input.txt"
           let myLines = lines input
           putStrLn $ "the input was " ++ show myLines ++ " and goodnight"
           let example1 = nice "ugknbfddgicrmopn"
           putStrLn $ "example 1  = " ++ show example1 ++ " and goodnight"
           let example2 = nice "jchzalrnumimnmhp"
           putStrLn $ "example 2  = " ++ show example2 ++ " and goodnight"
           let example3 = nice "haegwjzuvuyypxyu"
           putStrLn $ "example 3  = " ++ show example3 ++ " and goodnight"
           let example4 = nice "dvszwmarrgswjxmb"
           putStrLn $ "example 4  = " ++ show example4 ++ " and goodnight"
           let part1 = solvePart1 myLines
           putStrLn $ "part 1  = " ++ show part1 ++ " and goodnight"
           let part2 = solvePart2 myLines
           putStrLn $ "part 2 = " ++ show part2 ++ " and goodnight"


{--
example 1  = True and goodnight
example 2  = False and goodnight
example 3  = False and goodnight
example 4  = False and goodnight
part 1  = 227 and goodnight


--}
