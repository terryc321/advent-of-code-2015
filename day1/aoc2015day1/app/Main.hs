

module Main(main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import qualified System.Directory as SD 

main :: IO ()
main = do
    -- Attempt to read the file
    input <- BS.readFile "../input"
    input2 <- (BS.drop 1 input)
    input3 <- (BS.dropEnd 1 input2) ??? 
    putStrLn $ input3 

    -- Verify we got something
    let count = BS.length input
    putStrLn $ "Read " ++ show count ++ " bytes."

    -- -- Optional: Print the first 50 bytes to visually confirm it's not garbage
    -- if count > 0 
    --     then do
    --         let preview = BSC.unpack (BS.take 50 input)
    --         putStrLn $ "Preview: " ++ preview
    --     else putStrLn "File is empty or path is wrong."


--     Could not load module ‘Data.ByteString’.
--     Could not load module ‘Data.ByteString.Char8’.


