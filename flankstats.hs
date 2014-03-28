--produces statistics on the probes grouped by core sequence
--for each core, reports the number of probes, mean intensity, median intensity, min intensity,
--max intensity, and standard deviation of the intensity for probes with that core

--usage: flankstats core=core_size < extracted microarray alldata file

--author: Tristan Bepler (tbepler@gmail.com)

--requires the vector and statistics packages on hackage

import Data.List
import System.Environment
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Statistics.Sample as Stats

--http://stackoverflow.com/a/16111081
import qualified Data.Set as Set
rmdups :: Ord a => [a] -> [a]
rmdups = rmdups' Set.empty where
	rmdups' _ [] = []
	rmdups' a (b:c) = if Set.member b a
		then rmdups' a c
		else b : rmdups' (Set.insert b a) c

data Probe = Probe {name :: String, sqnc :: String, intensity :: Double}
instance Read Probe where
	readsPrec _ s = [(Probe (tokens !! 0) (tokens !! 1) (read $ tokens !! 2), unwords $ drop 3 tokens)]
		where tokens = words s
instance Show Probe where
	show probe = unwords [name probe, sqnc probe, show $ intensity probe]

groupByCore :: Int -> [Probe] -> [(String, [Probe])]
groupByCore coreSize probes = groupByCore' coreSize probes Map.empty where
	groupByCore' :: Int -> [Probe] -> Map.Map String [Probe] -> [(String, [Probe])]
	groupByCore' coreSize [] probeMap = Map.toList probeMap
	groupByCore' coreSize (cur:remainder) probeMap = if Map.member coreSeq probeMap
		then groupByCore' coreSize remainder (Map.insert coreSeq (cur : (probeMap Map.! coreSeq)) probeMap)
		else groupByCore' coreSize remainder (Map.insert coreSeq [cur] probeMap)
		where coreSeq = core coreSize cur

core :: Int -> Probe -> String
core size probe = drop flank $ take (flank + size) (sqnc probe)
	where flank = ((length $ sqnc probe) - size) `div` 2

cores :: Int -> [Probe] -> [String]
cores size probes = rmdups $ map (core size) probes

stats :: [Probe] -> (Int, [Double])
stats probes = (num, [meanVal, medianVal, minVal, maxVal, stdDev])
	where
		listInten = map (intensity) probes
		inten = Vector.fromList listInten
		num = fromIntegral $ length listInten
		maxVal = roundDec 2 $ Vector.maximum inten
		minVal = roundDec 2 $ Vector.minimum inten
		meanVal = roundDec 2 $ Stats.mean inten
		medianVal = roundDec 2 $ median listInten
		stdDev = roundDec 2 $ Stats.stdDev inten

roundDec :: Int -> Double -> Double
roundDec dec num = (fromIntegral $ round (num * 10^^dec)) / (fromIntegral 10^^dec)

median :: [Double] -> Double
median xs = if length xs `mod` 2 == 0
	then ((sorted !! middle) + (sorted !! (middle - 1))) / 2.0
	else sorted !! middle
	where
		sorted = sort xs
		middle = ((length xs) `div` 2)

coreStats :: [(String, [Probe])] -> [(String, (Int, [Double]))]
coreStats xs = map (\(coreSeq, probes) -> (coreSeq, stats probes)) xs

statHeader :: String
statHeader = unwords ["Core", "Count", "Mean", "Median", "Min", "Max", "StdDev"]

statsToString :: [(String, (Int, [Double]))] -> String
statsToString stats = unlines $ [statHeader] ++ map (statToString) stats

statToString :: (String, (Int, [Double])) -> String
statToString (coreSeq, (count, stats)) = unwords $ [coreSeq] ++ [(show count)] ++ (map (show) stats)

toString :: [(String, [Probe])] -> String
toString ((coreSeq, probes):xs) = coreSeq ++ "\n" ++ (unlines $ map (show) probes) ++ "\n" ++ toString xs

main = do
	args <- getArgs
	let coreSize = if length args >= 1 then read $ head args else error "No core size specified."
	interact(\x-> statsToString $ coreStats $ groupByCore coreSize $ map (read) $ lines x)