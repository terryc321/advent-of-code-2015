
import System.Environment as SE

slen :: String -> Int 
slen s = length s 

part1 :: String -> Int
part1 s = part1b s 0

-- skip whitespace anything not ( ) 
part1b :: String -> Int -> Int
part1b [] n = n
part1b ('(' : xs) n = part1b xs (n + 1)
part1b (')' : xs) n = part1b xs (n - 1)
part1b (_ : xs) n = part1b xs n


part2 :: String -> Int
part2 s = part2b s level position
  where level = 0
        position = 1
        

-- skip whitespace anything not ( )
-- when n dips below 0 then report position of character
-- positions start at 1 
part2b :: String -> Int -> Int -> Int
part2b [] n p = p
part2b ('(' : xs) n p = part2b xs (n + 1) (p + 1)
part2b (')' : xs) n p = if (n - 1) < 0 then p else part2b xs (n - 1) (p + 1)
part2b (_ : xs) n p = part2b xs n p

                      


main :: IO ()
main = do putStrLn "Please provide input filename "
          putStrLn ""
          args <- SE.getArgs
          let arg1 = args !! 0
          putStrLn ("argument 1 is [ " ++ (show arg1) ++ "]")
          file <-readFile arg1
          putStrLn file
          let len = slen file
          putStrLn ("the file is " ++ (show len) ++ " characters long")
          let sol1 = part1 file
          putStrLn ("solution part1 is [ " ++ (show sol1) ++ " ] ")          
          let sol2 = part2 file
          putStrLn ("solution part2 is [ " ++ (show sol2) ++ " ] ")          
          putStrLn "done"
          
          
--           let args = getArgs 
--           putStrLn "we got " ++ (show (args !! 0))
       -- let content = readFile (args !! 0)




