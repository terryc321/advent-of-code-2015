module Lib
    ( solvePart1 , isNice , threeVowels , avoidPair , twiceInRow 
    ) where

threeVowelsRec :: Int -> [Char] -> Int
threeVowelsRec n [] = n
threeVowelsRec n (h : t ) =
  case h of
    'a' -> threeVowelsRec (n + 1) t
    'e' -> threeVowelsRec (n + 1) t
    'i' -> threeVowelsRec (n + 1) t
    'o' -> threeVowelsRec (n + 1) t
    'u' -> threeVowelsRec (n + 1) t
    _ ->   threeVowelsRec n t

threeVowels :: [Char] -> Bool
threeVowels s = threeVowelsRec 0 s >= 3 

twiceInRow :: [Char] -> Bool
twiceInRow [] = False
twiceInRow [_] = False
twiceInRow (x : y : t ) = (x == y) || twiceInRow (y : t)


avoidPair :: [Char] -> Bool
avoidPair [] = True
avoidPair [_] = True
avoidPair (x : y : t) =
    case (x, y) of
        ('a', 'b') -> False
        ('c', 'd') -> False
        ('p', 'q') -> False
        ('x', 'y') -> False
        (_ , _) -> avoidPair (y : t)

isNice :: [Char] -> Bool
isNice s = let a = threeVowels s
               b = twiceInRow s
               c = avoidPair s
           in a && b && c


solvePart1 :: [[Char]] -> Int 
solvePart1 xs = length $ filter isNice xs

-- solvePart2 xs = 456

-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

-- import Data.Text (Text)
-- import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
-- import qualified Data.Text.Encoding as TE
-- import qualified Text.Read as TR
-- import Data.List (sort)
-- import Data.Map.Strict (Map)
-- import qualified Data.Map.Strict as Map

-- import Data.ByteString (ByteString)
-- import qualified Data.ByteString.UTF8 as UTF8
-- import qualified Data.ByteString as BS
-- -- import qualified Data.ByteString.Lazy as BL
-- import Data.Digest.Pure.MD5 as MD

-- import qualified Data.ByteString.Lazy.Char8 as BL
-- -- import qualified Data.ByteString.Lazy as BLStrict -- Optional, if you need strict conversion later

-- -- | Read a file and return a list of lines as Lazy ByteStrings
-- readFileLines :: FilePath -> IO [BL.ByteString]
-- readFileLines path = do
--   content <- BL.readFile path
--   -- 'lines' splits on '\n' (newline) characters
--   return (BL.lines content)

-- day5 read input in as bytestring then we can do some stuff with them 
-- non overlapping pair
-- ab ab
-- ab . ab
--                        ab . . ab
-- given a pair at i      |
-- check forward if 
--


