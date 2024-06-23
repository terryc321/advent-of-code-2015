-- language directives at top of file
{-# LANGUAGE QuasiQuotes #-}

-- mdoule declaration
--    containing import statements
--    followed by definitions

module MyLib2 (someFunc  ) where

import Input

-- https://kseo.github.io/posts/2014-02-06-multi-line-strings-in-haskell.html
import Text.RawString.QQ
-------------------------------------------------------------------------------
-- no more imports 
-------------------------------------------------------------------------------

-- embedded puzzle input directly into haskell source file
-- avoided interpolation 
multi :: String
multi = [r|"sjdivfriyaaqa\xd2v\"k\"mpcu\"yyu\"en"
"vcqc"
"ccxjgiocmuhf\"ycnh"
"lltj\"kbbxi"|]

remd [] s = s
remd (x:t) s = case member x s of
                True -> remd t s
                False -> remd t (x : s)

member _ [] = False
member x (y : t) = if x == y then True
                   else member x t
                   
--------------------------------------------------------------------------  

-- mol = mole

a = [ y | (x,y) <- subs , ['e'] == x ] 


{- 
    g2c = [ let lim = (length mol) - 2
                rg  = [0 .. lim]
                q   = \x3 y3 z3 -> let to = take 2 (drop x3 mol)
                                    in if to == y3 then
                                        let a2 = take x3 mol
                                            z2 = drop (x3 + 2) mol
                                        in a2 ++ z3 ++ z2
                                    else []
            in [ q x po go | x <- rg ]  |  (po , go) <- subs ]

    g2b = foldr (++) [] g2c
    g2 = filter (\x -> (not (x == ""))) g2b

    g1c  = [ let lim = (length mol) - 1
                rg  = [0 .. lim]
                q   = \x3 y3 z3 -> let to = take 1 (drop x3 mol)
                                    in if to == y3 then
                                        let a2 = take x3 mol
                                            z2 = drop (x3 + 1) mol
                                        in a2 ++ z3 ++ z2
                                    else []
            in [ q x po go | x <- rg ]  |  (po , go) <- subs ]

    g1b = foldr (++) [] g1c
    g1 = filter (\x -> (not (x == ""))) g1b

    g = g1 ++ g2

 -}

{- 
sol = remd g []

sol_count = length sol
 -}


someFunc :: IO ()
someFunc =  do putStrLn ("Hello haskell , from MyLib2 ")
               putStrLn "  "
               putStrLn "  TO DO solution ... "
               ---putStrLn ("There are " ++ show sol_count ++ " different molecules ")
               putStrLn "  "
               putStrLn "  "
               return ()


