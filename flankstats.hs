--produces statistics on the probes grouped by core sequence

--usage: flankstats core=core_size < extracted microarray alldata file

--author: Tristan Bepler (tbepler@gmail.com)

import Data.List
import System.Environment

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
groupByCore coreSize probes = zip coreSeqs $ map (\x-> filter (\y-> x == (core coreSize y)) probes) coreSeqs
	where coreSeqs = cores coreSize probes

core :: Int -> Probe -> String
core size probe = drop flank $ take (flank + size) (sqnc probe)
	where flank = ((length $ sqnc probe) - size) `div` 2

cores :: Int -> [Probe] -> [String]
cores size probes = rmdups $ map (core size) probes

stats :: [Probe] -> [Double]
stats probes = [meanVal, medianVal, minVal, maxVal, stdDev]
	where 
		inten = map (intensity) probes
		maxVal = maximum inten
		minVal = minimum inten
		meanVal = mean inten
		medianVal = median inten
		stdDev = stddevp inten

mean :: [Double] -> Double
mean xs = (sum xs) / (fromIntegral (length xs))

median :: [Double] -> Double
median xs = if length xs `mod` 2 == 0
	then ((sorted !! middle) + (sorted !! (middle - 1))) / 2.0
	else sorted !! middle
	where
		sorted = sort xs
		middle = ((length xs) `div` 2)

stddevp :: [Double] -> Double
stddevp xs = sqrt (sum $ map (\x-> ((x-m)**2)) xs) / (fromIntegral (length xs))
	where m = mean xs

coreStats :: [(String, [Probe])] -> [(String, [Double])]
coreStats xs = map (\(coreSeq, probes) -> (coreSeq, stats probes)) xs

statHeader :: String
statHeader = unwords ["Core", "Mean", "Median", "Min", "Max", "StdDev"]

statsToString :: [(String, [Double])] -> String
statsToString stats = unlines $ [statHeader] ++ map (statToString) stats

statToString :: (String, [Double]) -> String
statToString (coreSeq, stats) = unwords $ [coreSeq] ++ (map (show) stats)

toString :: [(String, [Probe])] -> String
toString ((coreSeq, probes):xs) = coreSeq ++ "\n" ++ (unlines $ map (show) probes) ++ "\n" ++ toString xs

main = do
	args <- getArgs
	let coreSize = if length args >= 1 then read $ head args else error "No core size specified."
	interact(\x-> statsToString $ coreStats $ groupByCore coreSize $ map (read) $ lines x)