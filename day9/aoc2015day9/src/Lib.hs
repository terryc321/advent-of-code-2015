module Lib
    (
    Trip(..)
    , theTrips
    , allPlaces
    , distances
    , look
    , permutation
    , development
    , emeasure
    , minBySecond
    , part1
    , part2
    ) where

{--
London to Dublin = 464
London to Belfast = 518
Dublin to Belfast = 141

split based on blank
{0place   1to    {2place}    3=    {4distance}
                                     Int 
elements !! 0 2 and 4 

--}

-- import Data.Map (Map)
import qualified Data.Map as M

import Data.List (List)
import qualified Data.List as L


data Trip = Trip
 { from :: String
 , to :: String
 , dist :: Int
 } deriving (Show,Eq)


theTrips :: String -> [Trip] 
theTrips input = let sp = lines input
                 in map rd sp
                        
distances :: [Trip] -> M.Map (String,String) Int
distances xs = let ys = map (\trip ->  let mfrom = from trip
                                           mto = to trip
                                           mdist = dist trip
                                       in ((mfrom,mto),mdist)) xs
                   zs = map (\trip ->  let mfrom = from trip
                                           mto = to trip
                                           mdist = dist trip
                                       in ((mto,mfrom),mdist)) xs                        
               in M.fromList (ys ++ zs)
       
rd :: String -> Trip
rd s = let w = words s
       in let mfrom = w !! 0
              mto = w !! 2
              mdist = read (w !! 4) :: Int
          in Trip
             { from = mfrom
             , to = mto
             , dist = mdist
             }

-- reimplement a set              
-- pattern match on a 
allPlacesR :: [Trip] -> [String] -> [String]
allPlacesR [] ss = ss 
allPlacesR (h : t) ss = let mfrom = from h
                            mto = to h 
                        in let aout = not $ elem mfrom ss
                               bout = not $ elem mto ss
                           in
                             if (aout && bout) then allPlacesR t (mfrom : mto : ss)
                             else if aout then allPlacesR t (mfrom : ss)
                                  else if bout then allPlacesR t (mto : ss)
                                       else allPlacesR t ss 

                      
allPlaces :: [Trip] -> [String]
allPlaces ts = allPlacesR ts [] 

{--

the places are ["Arbre","Straylight","Norrath","Faerun","Tambi","Snowdin","Tristram","AlphaCentauri"]

distances from allPlaces 

iterate over places

f1 start destinations trips dist_travelled
 -> for each destination
     make a trip from start -> destination
     add it to trips
     increment dist_travelled by how far start to destination
     continue with f1 destination (destinations without destination) trips2 dist_travelled2

initially
--}

--- IO !!
-- look :: String -> [String] -> (M.Map (String,String) Int) -> Int ->  IO () 
-- look d [] map ti = ti -- no more destinations
-- look d (h : t) map ti = case M.lookup (d,h) map of
--                           Just d -> 
--                           Nothing -> 


look :: String -> (M.Map (String,String) Int) -> [String] -> [String] -> Int -> IO ()
look s emap []      acc n = putStrLn $ "" ++ show acc ++ " => " ++ show n -- write something
look s emap dests acc n = let _ =  map (\d ->
                                         case M.lookup (s,d) emap of
                                           Just x -> look d emap (L.delete d dests) (s : acc) (n + x)
                                           Nothing -> putStr "" -- write nothing
                                       ) dests
                          in putStr "" -- write nothing




-- fx :: [String] -> [([String],[String])]
fx xs = map (\s -> ([s],L.delete s xs)) xs

gx xs = (foldr (++) [] (map (\pr -> let (as,bs) = pr in map (\s -> (s : as , L.delete s bs)) bs) xs))

hx xs = let r = head xs
        in let (as,bs) = r
           in if bs == [] then map (fst) xs -- takes 1st elems only of tuples
              else hx (gx xs) -- re-run xs 

permutation xs = hx (fx xs)


-- hx xs = let (as,bs) = head xs
--         in if bs == [] then xs
--            else hx (gx xs)

-- measure distance of a trip
emeasure :: [String] -> (M.Map (String,String) Int) -> Int -> Int
emeasure []  mm d  = d
emeasure [ _ ] mm d = d
emeasure (a : b : t) mm d = case M.lookup (a,b) mm of
                             Just x -> emeasure (b : t) mm (d + x )
                             Nothing -> -999

-- development :: String -> [([String], Int)]
development input =  let trips = theTrips input
                     in let places = allPlaces trips
                            distMap = distances trips
                        in  let routes = permutation places
                            in map (\route -> (route , emeasure route distMap 0)) routes 


minBySecond :: ([String], Int) -> ([String], Int) -> ([String], Int)
minBySecond a b = 
    if (snd a) <= (snd b)
    then a 
    else b

{--
find the lowest solution
filter for any solutions that are also the lowest 
--}
part1 input =
  let out = (development input)
  in let low = foldr (minBySecond) (["dummy","path","should","not","see"],999999999) out
     in let (_ , score) = low
              in filter (\s -> (snd s) == score) out

--  let (_,score) = low in filter (\s -> (snd s) == score) low 

part2 input = 456

              
                             
                             
  
                          

