module Main(main) where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

-- import qualified System.Directory as SD
-- import qualified Data.Char as DC 
import Data.Char(ord)


-- isOpen = \x -> x == ord '(' 
-- isClose = \x -> x == ord ')'

walk :: BS.ByteString -> Int -> Int -> Int 
walk bs pos depth = let w = BS.index bs pos 
     in if w == fromIntegral ( ord '(') then walk bs (pos + 1) (depth + 1)
        else let d = depth - 1  
             in if d < 0 then pos 
                else walk bs (pos + 1) d 
                  

-- using ByteString , need add bytestring as dependency in package.yaml 
main :: IO ()
main = do
    input <- BS.readFile "../input"
    let input2 = BS.drop 1 input
    let input3 = BS.dropEnd 1 input2 
    putStrLn $ BSC.unpack input3
    putStrLn $ "your input file is " ++ show (BS.length input3) ++ " bytes long"
    let ups = BS.length (BS.filter (\x -> x == fromIntegral (ord '(')) input3) 
    let downs = BS.length (BS.filter (\x -> x == fromIntegral (ord ')')) input3)   
    -- let ups = BS.length (BS.filter (\x -> x == 40) input3) 
    -- let downs = BS.length (BS.filter (\x -> x == 41) input3)   
    let level = ups - downs
    putStrLn $ "the final level santa is " ++ show (level) ++ " and goodnight"
    let basement = walk input3 0 1 
    putStrLn $ "santa entered the basement at index " ++ show (basement) ++ " and goodnight"

        

{--

readFile :: FilePath -> IO ByteString
drop :: Int -> ByteString -> ByteString
dropEnd :: Int -> ByteString -> ByteString
putStrLn :: String -> IO 

-- input2 <- (BS.drop 1 input)
-- input3 <- (BS.dropEnd 1 input2)
-- putStrLn $ BSC.unpack input3

-- -- Verify we got something
-- let count = BS.length input
-- putStrLn $ "Read " ++ show count ++ " bytes."

-- -- Optional: Print the first 50 bytes to visually confirm it's not garbage
-- if count > 0
--     then do
--         let preview = BSC.unpack (BS.take 50 input)
--         putStrLn $ "Preview: " ++ preview
--     else putStrLn "File is empty or path is wrong."

--     Could not load module ‘Data.ByteString’.
--     Could not load module ‘Data.ByteString.Char8’.
--} 
