--this script combines the replicates from PBMs by taking the median of the fwd and rvs
--replicates

import System.Environment
import System.IO
import Data.Maybe
import Data.List
import Data.List.Split
import qualified Probe as Probe
import qualified DNA as DNA
import qualified Data.Map as Map

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

select :: Ord a => Int -> [a] -> a
select n xs = (quicksort xs) !! n

percentile :: (Fractional a, Ord a, RealFrac b) => b -> [a] -> a
percentile p [] = error "Cannot find the percentil of an empty list"
percentile p xs = ((select top xs) + (select bot xs)) / 2 where
	top = ceiling ((fromIntegral ((length xs) - 1)) * p) 
	bot = floor ((fromIntegral ((length xs) - 1)) * p) 

median :: (Fractional a, Ord a) => [a] -> a
median [] = error "Cannot find the median of an empty list"
median [x] = x
median xs = if odd $ length xs then selodd else seleven where
	selodd = select mid xs
	seleven = ((select mid xs) + (select (mid-1) xs)) / 2
	mid = (length xs) `div` 2

iqr :: (Fractional a, Ord a) => [a] -> a
iqr [] = error "Cannot find the IQR of an empty list"
iqr xs = (percentile 0.75 xs) - (percentile 0.25 xs)

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

parseProbe :: String -> (String, String, [Double])
parseProbe line = parse' $ words line where
	parse' (name:sqnc:xs) = (name, sqnc, map (read) xs)

parseProbes :: [String] -> [(String, String, [Double])]
parseProbes xs = map (parseProbe) xs

parseReplicate :: String -> Int
parseReplicate name = index where
	index = if isJust token then read $ tail $ fromJust token else 1
	token = find (match) $ splitOn "_" name
	match ('r' : num) = True
	match xs = False

probeKey :: (String, String, [Double]) -> String
probeKey (name, sqnc, scores) = if rep < 4 then sqnc else DNA.rvscmpl sqnc
	where rep = parseReplicate name

getConditions :: [(String, String, [Double])] -> [[Double]]
getConditions [] = []
getConditions [(_,_,values)] = map (\x -> [x]) values
getConditions ((name, sqnc, values) : xs) = map (\(x,y)-> x:y) $ zip values next
	where next = getConditions xs

combine :: (String, [(String, String, [Double])]) -> (String, String)
combine (key, probes) = (
	unwords $ [key, commonPrefix names] ++ (combine' fwdMed rvsMed dif jointMed),
	reportCondition key (commonPrefix names) (length probes) fwdMed rvsMed dif jointMed
	)
	where
		names = (map (\(x,_,_)->x) probes)
		fwdProbes = filter (\(name, sqnc, values)-> key == sqnc) probes
		rvsProbes = filter (\(name, sqnc, values)-> key /= sqnc) probes
		fwdCond = getConditions fwdProbes
		rvsCond = getConditions rvsProbes
		combinedCond = getConditions probes
		fwdMed = map (median) fwdCond
		rvsMed = map (median) rvsCond
		dif = map (\(x,y) -> abs (x-y)) $ zip fwdMed rvsMed
		jointMed = map (median) combinedCond
 
combine' :: [Double] -> [Double] -> [Double] -> [Double] -> [String]
combine' fwdMed rvsMed dif jointMed = map (\(a,b,c,d)-> combine'' a b c d) $ zip4 fwdMed rvsMed dif jointMed  where
	combine'' w x y z
		| y >= 0.5 = if w > x then show w else show x
		| otherwise = show z

combineReps :: [(String, [(String, String, [Double])])] -> [(String,String)]
combineReps probeGroups = map (combine) probeGroups


reportCondition :: String -> String -> Int -> [Double] -> [Double] -> [Double] -> [Double] -> String
reportCondition sqnc name occurences fwdMed rvsMed dif jointMed = unwords $ [sqnc, name, show occurences] ++ vals where
	vals = concatMap (\(w,x,y,z)->[show w, show x, show y, show z]) $ zip4 fwdMed rvsMed dif jointMed

reportHeader :: [String] -> (String, [(String, String, [Double])]) -> String
reportHeader header (_, probes) = unwords $ ["Sequence", "Name", "Occurences"] ++ values where
	values = concatMap (\x-> [x++"_Fwd_Median", x++"_Rvs_Median", x++"_Diff", x++"_Combined_Median"]) header

main = do
	args <- getArgs
	reportTo <- if length args > 0 then openFile (head args) WriteMode else openFile "combinereps.log" WriteMode
	input <- getContents
	let
		(h : body) = lines input
		header = tail $ tail $ words h
		probeGroups = Main.groupBy probeKey $ parseProbes body
		(output, report) = unzip $ combineReps probeGroups
	hPutStrLn reportTo $ reportHeader header $ head probeGroups
	hPutStrLn reportTo $ unlines report
	hClose reportTo
	putStrLn h
	putStrLn $ unlines output





