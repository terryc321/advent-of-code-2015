

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


solvePart1 x = MD.md5 x
solvePart2 x = 0 



-- using pureMD5 package 
-- MD5("The quick brown fox jumps over the lazy dog") = "9e107d9d372bb6826bd81d3542a419d6"
-- MD5("The quick brown fox jumps over the lazy dog.") = "e4d909c290d0fb1ca068ffaddf22cbd0"
-- MD5("") = "d41d8cd98f00b204e9800998ecf8427e"

-- input has extra double quotes at start and end of input 
main :: IO ()
main = do  let input = stringToLazyByteString "The quick brown fox jumps over the lazy dog"
           let part1 = solvePart1 input
           putStrLn $ "part 1 solution " ++ show part1 ++ " and goodnight"
           let input2 = stringToLazyByteString "The quick brown fox jumps over the lazy dog."
           let part2 = solvePart2 input2
           putStrLn $ "part 2 solution " ++ show part2 ++ " and goodnight"
        
