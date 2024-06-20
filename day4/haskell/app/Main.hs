module Main where

--- simpler md5 
import qualified Data.Hash.MD5 as SMD5

import qualified Data.Char as CH

--- dependency as bytestring
import qualified Data.ByteString as BS

--- dependency as cryptohash-md5
import qualified Crypto.Hash.MD5 as MD5

import Numeric (showHex)

convertToHex2 :: (Integral a, Show a) => a -> String
convertToHex2 x = showHex x ""

convertToHex x = let s = convertToHex2 x
                     t = map CH.toUpper s
                 in case length t of
                    2 -> t
                    1 -> "0" ++ t
                    0 -> "00"

foo = BS.unpack digest  where
            digest = MD5.finalize ctx
            ctx    = foldl MD5.update ctx0 (map BS.pack [ [1,2,3], [4,5,6] ])
            ctx0   = MD5.init

foo2 = foldr (++) (""::String) (map convertToHex foo)

foo3 = SMD5.md5s (SMD5.Str "abcdef609043")

foo4 = SMD5.md5s (SMD5.Str "pqrstuv1048970")

-- te = trial and error
te :: String -> Integer -> Integer
te s n = let s2 = s ++ (show n)
             md = SMD5.md5s (SMD5.Str s2)
             t5 = take 5 md
             all5 = t5 == "00000"
         in if all5 then n
            else te s (n + 1)

fix :: String -> Integer
fix s = te s 0

solution1 = fix "bgvyzdsv"


main :: IO ()
main = do putStrLn "Hello, Haskell!"
          putStrLn $ "solution part 1 is [ " ++ (show solution1) ++ " ] "
          -- print foo


-- terry@debian:~/code/advent-of-code/advent-of-code-2015/day4/haskell$ cabal build
-- Up to date
-- terry@debian:~/code/advent-of-code/advent-of-code-2015/day4/haskell$ time cabal run
-- Hello, Haskell!
-- solution part 1 is [ 254575 ] 
-- 
-- real	0m0.785s
-- user	0m0.771s
-- sys	0m0.011s


