
module Main (main) where

import Debug.Trace  -- debugging 

import Data.List(isInfixOf)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Word (Word16)
import Data.Char (isDigit)

import Wire (Operation(..) , Source(..))
import Data.Bits ((.&.), (.|.) , shiftL , shiftR , complement)
-- (.&.) is AND, (.|.) is OR


{--
stack repl

we can load a file and ignore the fact its an io string
str <- readFile "../input.txt" ;
let lines' = lines str ;
let pp = map myparse lines'
let mm = Map.fromList pp

-- look up values using Map.lookup "z" mm
-- returns a Just x or Nothing
--}

-- not is only odd one out , other commands have same structure 
-- NOT kt -> ku
--  0  1  2   3

-- lf AND    lq -> ls
-- bo OR     bu -> bv
-- ip LSHIFT 15 -> it
-- iu RSHIFT 1  -> jn
-- 0   1     2   3  4

myparse :: String -> (String, Operation)
myparse str =
    let w = words str
        isSym = (\s -> isInfixOf s str) -- check if sym is in string str 
        mkW = (\n -> Wire (w !! n)) -- build  Wire (nth-element of word list)
        mkL = (\n -> read (w !! n) :: Int) -- build Literal 
        sk = (\n -> w !! n) -- make key taking nth element from word list w 
     in if (w !! 0) == "NOT" then (sk 3 , Not (mkW 1))
        else if (w !! 1) == "LSHIFT" then (sk 4 , Lshift (mkW 0) (mkL 2))
        else if (w !! 1) == "RSHIFT" then (sk 4 , Rshift (mkW 0) (mkL 2))
        else if (w !! 1) == "AND"    then if isDigit ((w !! 0) !! 0) then  (sk 4 , And (Literal (read (w !! 0) :: Word16)) (mkW 2))
                                          else (sk 4 , And (mkW 0) (mkW 2))                            
        else if (w !! 1) == "OR"     then (sk 4 ,    Or  (mkW 0) (mkW 2))
        else if (w !! 1) == "->"     then
               if isDigit ((w !! 0) !! 0) then (sk 2 , Direct (Literal (read (w !! 0) :: Word16)))
               else (sk 2 , Direct (mkW 0))
             else ("" , Ufo w)

type ValueMap = Map String Word16  

-- t : target
-- mp : myparse - list of keys and operations - e.g ("ls",And (Wire "lf") (Wire "lq"))
-- vm : value map - keys strings : values are Word16 unsigned 16 bit integers  "ls" 3
-- calls resolve , iterate all over parsed operations , try to find if key is resolved - move on
-- if key not resolved are the two legs both already resolved in value map
-- if so we can compute operation - then put result into value map
-- so we make progress
-- we do redundant work perhaps by not shortcutting as soon as symbol is resolved
-- maybe CPS could help 
resolveTarget :: String -> [(String , Operation)] -> ValueMap -> Word16
resolveTarget t mp vm =  
   let vm2 = resolve mp vm
   in case Map.lookup t vm2 of
        Just x -> x
        _ -> resolveTarget t mp vm2

             


-- list of tuples String , Operation  and Map String Operation 
resolve :: [(String , Operation)] -> ValueMap -> ValueMap 
resolve [] vm = vm
resolve (h : t ) vm = let (sym , op) = h 
                      in case Map.lookup sym vm of 
                           Just x -> resolve t vm  -- already resolved
                           Nothing -> let vt = tryResolve sym op vm
                                      in resolve t vt

tryResolve :: String -> Operation -> ValueMap -> ValueMap 
tryResolve sym op vm =
  case op of
    And (Wire s1) (Wire s2) -> andResolve sym op s1 s2 vm
    And (Literal w1) (Wire s2) -> andwResolve sym op w1 s2 vm 
    Or (Wire s1) (Wire s2) -> orResolve sym op s1 s2 vm 
    Lshift (Wire s1) i2 -> lshiftResolve sym op s1 i2 vm 
    Rshift (Wire s1) i2 -> rshiftResolve sym op s1 i2 vm    
    Not (Wire s1) -> notResolve sym op s1 vm 
    Direct (Wire s1) -> directwResolve sym op s1 vm 
    Direct (Literal w1) -> directlResolve sym op w1 vm
    _ -> trace ("tryResolve shucks " ++ show op ++ "\n") $  vm -- else give up 

andwResolve :: String -> Operation -> Word16 -> String -> ValueMap -> ValueMap  
andwResolve sym op w1 s2 vm =
  let v2 = Map.lookup s2 vm
  in case v2 of
    Just y -> let val =  (w1 .&. y)
                         in trace ("AND " ++ show op ++ " <== " ++ show val ++ "\n") $ Map.insert sym val vm
    Nothing -> vm
    


-- if string s1 is resolved then string sym is resolved also
-- simple pass through puts key into value map 
directwResolve :: String -> Operation -> String -> ValueMap -> ValueMap 
directwResolve sym op s1 vm =
    let v1 = Map.lookup s1 vm
    in case v1 of
      Just x  -> trace ("directw " ++ show op ++ " <== " ++ show x ++ "\n") $ Map.insert sym x vm
      _ -> vm 

--- easiest no resolving to do literally given the answer
--- a direct wire 
directlResolve :: String -> Operation -> Word16 -> ValueMap -> ValueMap 
directlResolve sym op w1 vm = trace ("directw " ++ show op ++ " <== " ++ show w1 ++ "\n" ) $ Map.insert sym w1 vm 

  
andResolve :: String -> Operation -> String -> String -> ValueMap -> ValueMap  
andResolve sym op s1 s2 vm =
  let v1 = Map.lookup s1 vm
      v2 = Map.lookup s2 vm
  in case (v1,v2) of
    (Just x , Just y) -> let val =  (x .&. y)
                         in trace ("AND " ++ show op ++ " <== " ++ show val ++ "\n") $ Map.insert sym val vm
    (Just x , Nothing) ->  trace ("AND " ++ show op ++ " only " ++ show s1 ++ " has value " ++ show v1 ++ "\n") $ vm
    (Nothing ,Just x) ->  trace ("AND " ++ show op ++ " only " ++ show s2 ++ " has value " ++ show v2 ++ "\n") $ vm
    (Nothing ,Nothing) -> vm 
    _ -> trace ("AND " ++ show op ++ " wacko \n") $ vm 

      
orResolve :: String -> Operation -> String -> String -> ValueMap -> ValueMap  
orResolve sym op s1 s2 vm =
  let v1 = Map.lookup s1 vm
      v2 = Map.lookup s2 vm
  in case (v1,v2) of
    (Just x , Just y) -> let val =  (x .|. y)
                         in trace ("OR " ++ show op ++ " <== " ++ show val ++ "\n") $ Map.insert sym val vm
    _ -> vm 

notResolve :: String -> Operation -> String -> ValueMap -> ValueMap  
notResolve sym op s1 vm =
  let v1 = Map.lookup s1 vm
  in case v1 of
    Just x  -> let val = complement x
               in trace ("NOT " ++ show op ++ " <== " ++ show val ++ "\n") $ Map.insert sym (complement x) vm
    _ -> vm 
      
lshiftResolve :: String -> Operation -> String -> Int -> ValueMap -> ValueMap  
lshiftResolve sym op s1 i2 vm =
  let v1 = Map.lookup s1 vm
  in case v1 of
    Just x  -> let val = (shiftL x i2)
               in trace ("LShift " ++ show op ++ " <== " ++ show val ++ "\n") $ Map.insert sym val vm
    _ -> vm 


rshiftResolve :: String -> Operation -> String -> Int -> ValueMap -> ValueMap  
rshiftResolve sym op s1 i2 vm =
  let v1 = Map.lookup s1 vm
  in case v1 of
    Just x  -> let val = (shiftR x i2)
               in trace ("RShift " ++ show op ++ " <== " ++ show val ++ "\n") $ Map.insert sym val vm
    _ -> vm 
      

  

                  
-- parser for AND expressions 
-- "lf AND lq -> ls"
-- symbol is any continuous characters , we could simply split the string
-- wm wire map - shows connections
-- vm value map - shows what values we know so far

-- mp and vm 
main :: IO ()
main = do input <- readFile "../input.txt"
          let split = lines input
          let mp = map myparse split
          let wm = Map.fromList mp
          -- let vm = Map.fromList (map (\s -> (fst s :: String,Nothing :: Maybe Operation)) mp)
          -- let vm = (Map.fromList []) :: Map String Operation
          let vm = Map.fromList [] :: ValueMap 
          putStrLn $ "lines read were " ++ show mp
          putStrLn $ "bad ones => " ++ show (filter (\s -> let (a,b) = s
                                                            in a == "") mp)
          putStrLn $ "value map => " ++ show vm
          let out = resolveTarget "a" mp vm
          putStrLn $ "wire on 'a' resolved to  => " ++ show out ++ "\n"

          
          
            

          
          
          

          

  

