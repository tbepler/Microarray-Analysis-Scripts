--this script combines the replicates from PBMs by taking the median of the fwd and rvs
--replicates

import Data.List.Split
import qualified Probe as Probe
import qualified Data.Map as Map

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

select :: Ord a => Int -> [a] -> a
select n xs = (quicksort xs) !! n

groupBy :: Ord k => (a -> k) -> [a] -> [(k, [a])]
groupBy fun ys = groupBy' fun ys Map.empty where
	groupBy' :: Ord k => (a -> k) -> [a] -> Map k [a] -> [(k, [a])]
	groupBy' fun [] m = Map.toList m
	groupBy' fun (x:xs) m = if Map.member key m
		then groupBy' fun xs (Map.insert key (x : (m Map.! key)) m)
		else groupBy' fun xs (Map.insert key [x] m)
		where key = fun x

parseProbe :: String -> (String, String, [Double])
parseProbe line = parse' $ words line where
	parse' (name:sqnc:xs) = (name, sqnc, map (read) xs)

parseReplicate :: String -> Int
parseReplicate name = index where
	Maybe index = token /= Nothing then read $ tail token else Maybe 1
	token = find (match) $ splitOn "_" name
	match ('r' : num) = True
	match xs = False






