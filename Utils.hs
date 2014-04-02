--this module provides some utility functions for lists

--author: Tristan Bepler (tbepler@gmail.com)

module Utils where

import Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set

rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
	rmdups' _ [] = []
	rmdups' a (b:c) = if Set.member b a
		then rmdups' a c
		else b : rmdups' (Set.insert b a) c


quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

select :: Ord a => Int -> [a] -> a
select n xs = (quicksort xs) !! n

range :: (Ord a, Num a) => [a] -> a
range xs = (maximum xs) - (minimum xs)

percentile :: (RealFrac a, Ord a) => a -> [a] -> a
percentile p [] = error "Cannot find the percentile of an empty list"
percentile p xs
	| p < (head ranks) = minimum xs
	| p > (last ranks) = maximum xs
	| index >= 0 = select index xs
	| otherwise = vbot + (p - pbot) * (vtop - vbot) / (ptop - pbot)
	where
		ranks = percentrank xs
		(Just index) = if findIndex (\x-> p == x) ranks /= Nothing then findIndex (\x-> p == x) ranks else Just (-1)
		ptop = minimum $ filter (\x-> x > p) ranks 
		pbot = maximum $ filter (\x-> x < p) ranks
		vtop = select n xs where (Just n) = findIndex (\x-> ptop == x) ranks
		vbot = select n xs where (Just n) = findIndex (\x-> pbot == x) ranks

percentrank :: (RealFrac b) => [a] -> [b]
percentrank xs = map (\n-> 100.0 * ((fromIntegral n) - 0.5) / (fromIntegral m)) [1..m] where m = length xs

median :: (Fractional a, Ord a) => [a] -> a
median [] = error "Cannot find the median of an empty list"
median [x] = x
median xs = if odd $ length xs then selodd else seleven where
	selodd = select mid xs
	seleven = ((select mid xs) + (select (mid-1) xs)) / 2
	mid = (length xs) `div` 2

iqr :: (RealFrac a, Ord a) => [a] -> a
iqr [] = error "Cannot find the IQR of an empty list"
iqr xs = (percentile 75.0 xs) - (percentile 25.0 xs)

commonPrefix :: Eq a => [[a]] -> [a]
commonPrefix [] = []
commonPrefix l = foldr1 commonPrefix' l where
	commonPrefix' (x:xs) (y:ys) | x == y = x : commonPrefix' xs ys
	commonPrefix' _ _ = []

groupBy :: Ord k => (a -> k) -> [a] -> [(k, [a])]
groupBy fun ys = groupBy' fun ys Map.empty where
	groupBy' :: Ord k => (a -> k) -> [a] -> Map.Map k [a] -> [(k, [a])]
	groupBy' fun [] m = Map.toList m
	groupBy' fun (x:xs) m = if Map.member key m
		then groupBy' fun xs (Map.insert key (x : (m Map.! key)) m)
		else groupBy' fun xs (Map.insert key [x] m)
		where key = fun x
