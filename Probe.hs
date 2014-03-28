module Probe where

import qualified DNA as DNA
import qualified Data.Map as Map

--http://stackoverflow.com/a/16111081
import qualified Data.Set as Set
rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
	rmdups' _ [] = []
	rmdups' a (b:c) = if Set.member b a
		then rmdups' a c
		else b : rmdups' (Set.insert b a) c

data Probe = Probe {
	name :: String,
	seq :: String,
	values :: [(String, Double)]
	}

groupBySeq :: [Probe] -> [(String, [Probe])]
groupBySeq [] = []
groupBySeq (x:xs) = groupByCore (length $ Probe.seq x) (x:xs)

groupByCore :: Int -> [Probe] -> [(String, [Probe])]
groupByCore coreSize probes = groupByCore' coreSize probes Map.empty where
	groupByCore' :: Int -> [Probe] -> Map.Map String [Probe] -> [(String, [Probe])]
	groupByCore' coreSize [] probeMap = Map.toList probeMap
	groupByCore' coreSize (cur:remainder) probeMap = if Map.member coreSeq probeMap
		then groupByCore' coreSize remainder (Map.insert coreSeq (cur : (probeMap Map.! coreSeq)) probeMap)
		else if Map.member rvsComp probeMap
			then groupByCore' coreSize remainder (Map.insert rvsComp (cur : (probeMap Map.! rvsComp)) probeMap)
			else groupByCore' coreSize remainder (Map.insert coreSeq [cur] probeMap)
		where 
			coreSeq = core coreSize cur
			rvsComp = DNA.rvscmpl coreSeq

readProbes :: String -> [String] -> [Probe]
readProbes header lines = map (read' . words) lines
	where
		colnames = drop 2 $ words header
		read' (n:s:xs) = Probe n s (zip colnames $ map (read) xs)
		

core :: Int -> Probe -> String
core n probe = drop flank $ take (flank + n) (Probe.seq probe)
	where flank = ((length $ Probe.seq probe) - n) `div` 2

cores :: Int -> [Probe] -> [String]
cores n probes = rmdups $ map (core n) probes


