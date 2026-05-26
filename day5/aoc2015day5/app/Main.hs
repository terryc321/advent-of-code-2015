
module Main(main) where 

import Lib

main :: IO ()
main = do  input <- readFile "../input.txt"
           let myLines = lines input
           let part1 = solvePart1 myLines
           putStrLn $ "part 1  = " ++ show part1 ++ " and goodnight"
           -- let part2 = solvePart2 myLines
           -- putStrLn $ "part 2 = " ++ show part2 ++ " and goodnight"


{--

           putStrLn $ "the input was " ++ show myLines ++ " and goodnight"
           let example1 = nice "ugknbfddgicrmopn"
           putStrLn $ "example 1  = " ++ show example1 ++ " and goodnight"
           let example2 = nice "jchzalrnumimnmhp"
           putStrLn $ "example 2  = " ++ show example2 ++ " and goodnight"
           let example3 = nice "haegwjzuvuyypxyu"
           putStrLn $ "example 3  = " ++ show example3 ++ " and goodnight"
           let example4 = nice "dvszwmarrgswjxmb"
           putStrLn $ "example 4  = " ++ show example4 ++ " and goodnight"

example 1  = True and goodnight
example 2  = False and goodnight
example 3  = False and goodnight
example 4  = False and goodnight
part 1  = 227 and goodnight


--}
