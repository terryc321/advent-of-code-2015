

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
import qualified Data.ByteString.Lazy as BL
import Data.Digest.Pure.MD5 as MD


stringToLazyByteString :: String -> BL.ByteString
stringToLazyByteString str = BL.fromStrict (TE.encodeUtf8 (T.pack str))

-- lazy string
iter :: String -> Int -> Int 
iter s n = let sl = stringToLazyByteString (s ++ show n)
           in let md5 = show (MD.md5 sl)
              in let take5 = take 5 md5
                 in if take5 == "00000" then n
                    else iter s (n + 1)

solvePart1 :: String -> Int 
solvePart1 s = iter s 0
               

iter2 :: String -> Int -> Int
iter2 s n =
    let sl = stringToLazyByteString (s ++ show n)
     in let md5 = show (MD.md5 sl)
         in let take6 = take 6 md5
             in if take6 == "000000"
                    then n
                    else iter2 s (n + 1)

solvePart2 :: String -> Int
solvePart2 s = iter2 s 0


-- using pureMD5 package 
-- MD5("The quick brown fox jumps over the lazy dog") = "9e107d9d372bb6826bd81d3542a419d6"
-- MD5("The quick brown fox jumps over the lazy dog.") = "e4d909c290d0fb1ca068ffaddf22cbd0"
-- MD5("") = "d41d8cd98f00b204e9800998ecf8427e"

-- input has extra double quotes at start and end of input 
main :: IO ()
main = do  let input = "bgvyzdsv"
           let example1 = solvePart1 "abcdef"
           let example2 = solvePart1 "pqrstuv"
           let part1 = solvePart1 input 
           putStrLn $ "example 1 = " ++ show example1 ++ " and goodnight"
           putStrLn $ "example 2 = " ++ show example2 ++ " and goodnight"
           putStrLn $ "part 1  = " ++ show part1 ++ " and goodnight"           
           let part2 = solvePart2 input
           putStrLn $ "part 2 solution " ++ show part2 ++ " and goodnight"
        
