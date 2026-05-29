module Lib
    ( someFunc,
      square,
      fLen ,
      memr ,
      encode 
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


-- rm -rf ~/.emacs.d/.lsp-session-v1 to reset lsp 
-- lsp-describe-session
-- lsp-workspace-folders-remove 
-- C-c l w d  -- workspace describe
-- C-c l w r  -- workspace 
data Color = Red | Green | Blue

describeColor :: Color -> String
describeColor Red = "red"
describeColor Green = "green"
describeColor Blue = "blue"


-- foo ::M aybe Int -> String
-- foo x = _     -- put cursor here

-- foo :: Maybe Int -> String
-- foo x = _

data Direction = SUp | SDown | SLeft | SRight

-- move :: Direction -> String
-- move _ 


double :: Int -> Int 
double x = x + x

triple :: Int -> Int
triple x = x + double x

square :: Int -> Int
square x = x * x 

half :: Int -> Int
half x = x `div` 2



-- given string - count the characters
fLen :: String -> Int
fLen s = length s 

-- given string - count the internal memory characters
mem :: String -> Int -> Int
mem [] n = n
mem ('\\' : 'x' : _ : _ : t) n = mem t (n + 1)
mem ('\\' : _ : t) n = mem t (n + 1)
mem (h : t ) n = mem t (n + 1)    


memr :: String -> Int
memr s = (mem s 0) - 2


encodeR :: String -> String
encodeR [] = []
encodeR ('"' : t) = '\\' : '"' : encodeR t
encodeR ('\\' : t) = '\\' : '\\' : encodeR t
encodeR (h : t) = h : (encodeR t)


encode :: String -> String
encode s = "\"" ++ (encodeR s) ++ "\""






  
