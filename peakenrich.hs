--this script takes peak files, sequences, and the genome and calculates the enrichment of the given
--sequences in the given peaks.

--author: Tristan Bepler (tbepler@gmail.com)

--Usage: peakenrich -s/--seqs=FILE -g/--genome=FILE NAME=PEAK_FILE...

{-# LANGUAGE BangPatterns #-}

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Data.Maybe (fromMaybe)
import Data.List
import Control.DeepSeq
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as Map
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Char as Char
import qualified Table as Table
import qualified Utils as Util
import qualified DNA as DNA

import Debug.Trace

data Flag
	= Help
	| Seqs String
	| Genome String
	| Distance Int
	deriving (Eq, Ord, Show)

flags = 
	[ 
	Option ['s'] ["seqs"] (ReqArg Seqs "FILE") "File containing sequences to calculate enrichment of",
	Option ['g'] ["genome"] (ReqArg Genome "FILE") "Genome file in fasta format",
	Option ['d'] ["distance"] (OptArg parseDistance "BASES") "Cutoff for distance between peaks to be considered in same region. Default = 0",
	Option ['h'] ["help"] (NoArg Help) "Print this help message"
	]

parse argv = case getOpt Permute flags argv of
	([], _, []) -> do 
					hPutStrLn stderr (usageInfo header flags)
					exitWith ExitSuccess
	(args, peakfilepaths , []) -> if elem Help args
				then do 
					hPutStrLn stderr (usageInfo header flags)
					exitWith ExitSuccess
				else return (seqfilepath, genomefilepath, peakfilepaths, cutoff xs)
				where
					((Seqs seqfilepath):(Genome genomefilepath):xs) = nub $ sort args
					cutoff [] = 0
					cutoff [Distance d] = d
					cutoff _ = 0
	(_,_,errs) -> do
			hPutStrLn stderr (concat errs ++ usageInfo header flags)
			exitWith (ExitFailure 1)
	where header = "Usage: peakenrich -s/--seqs=FILE -g/--genome=FILE NAME=PEAK_FILE..."

parseDistance :: Maybe String -> Flag
parseDistance (Nothing) = Distance 0
parseDistance (Just s) = Distance $ read s

parsePeakArg :: String -> (String, String)
parsePeakArg arg = parsePeakArg' False ("", "") arg where
	parsePeakArg' False (left, "") [] = (left, left)
	parsePeakArg' False (left, right) (x:xs) = if x == '=' then parsePeakArg' True (left, right) xs else parsePeakArg' False (left++[x], right) xs
	parsePeakArg' True (left, right) [] = (left, right)
	parsePeakArg' True (left, right) (x:xs) = parsePeakArg' True (left, right++[x]) xs
 
main = do
	(seqfilepath, genomefilepath, peakargs, cutoff) <- getArgs >>= parse
	--read the seqfile and extract the sequences column from the file
	seqfile <- readFile seqfilepath
	let seqs = Table.stringEntries $ fromMaybe (Table.StringCol "Sequence" []) $ Table.lookupCol "Sequence" $ Table.columns $ read seqfile
	let seqsalt = map (\x-> (Vector.fromList x, Vector.fromList $ DNA.rvscmpl x)) seqs
	--read the genome lazily as a bytestring
	genomefile <- B.readFile genomefilepath
	let genome = B.lines genomefile
	--read in the peaks, classify them, and store them in a map by chromosome
	let (peaknames, peakfilepaths) = unzip $ map (parsePeakArg) peakargs
	peakfiles <- mapM (readFile) peakfilepaths
	let inputpeaks = map (\(x,y)-> parsePeaks x y) $ zip peaknames peakfiles
	let inputpeakmap = foldl (combine) Map.empty inputpeaks
	--putStrLn $ unwords peaknames
	--putStrLn $ unwords $ map (show . Map.foldlWithKey (\b k a -> b + (length a)) 0) inputpeaks
	let peakmap = combine (classifyalt cutoff inputpeakmap) inputpeakmap
	--putStrLn $ unlines $ map (\(s, xs)-> unwords (s:(map (show) xs))) $ Util.groupBy classification $ expandPeakMap peakmap
	let (cls, count) = unzip $ map (\(s, xs) -> (s, show $ length xs)) $ Util.groupBy classification $ expandPeakMap peakmap
	--putStrLn $ unlines $ (unwords cls):[(unwords count)]
	putStrLn $ showEnrichmentScores $ enrichment genome seqs inputpeakmap
	--putStrLn $ showEnrichmentScores $ seqenrichment $ enrichmentalt genomefile seqsalt inputpeakmap

	

--peak = (classification, chromosome, start, end)
type Peak = (String, String, Int, Int)

classification :: Peak -> String
classification (c, _, _, _) = c

chromosome :: Peak -> String
chromosome (_, c, _, _) = c

start :: Peak -> Int
start (_, _, s, _) = s

end :: Peak -> Int
end (_, _, _, e) = e

peakspan :: Peak -> Int
peakspan (_, _, s, e) = e - s + 1

showEnrichmentScores :: [(String, Map.Map String (Int, Int))] -> String
showEnrichmentScores [] = ""
showEnrichmentScores ((s, m):xs) = unlines $ (unwords ("Sequence":keys)):(map (toString') ((s, m):xs)) where
	keys = Map.keys m
	toString' (sqnc, mapping) = unwords (sqnc:(map (getEnrichment' mapping) keys))
	getEnrichment' mapping key = show ((fromIntegral (inner+1))/(fromIntegral (outer+1)))  where
		(inner, outer) = fromMaybe (0,0) $ Map.lookup key mapping


byteStringToString :: B.ByteString -> String
byteStringToString bs
	| B.null bs = ""
	| otherwise = (B.head bs):(byteStringToString $ B.tail bs)

byteStringToVector :: B.ByteString -> Vector.Vector Char
byteStringToVector bs
	| B.null bs = Vector.empty
	| otherwise = Vector.cons (B.head bs) (byteStringToVector $ B.tail bs)

instance (NFData k, NFData a) => NFData (Map.Map k a) where
	 rnf = rnf . Map.toList

--takes the genome, sequences, and peaks and computes the enrichment of each sequence for each peak
enrichment :: [B.ByteString] -> [String] -> Map.Map String [Peak] -> [(String, Map.Map String (Int, Int))]
enrichment genome seqs peakMap = scores' where
	(scores', _, _, _) = foldl' (\(w,x,y,z) a-> w `seq` x `seq` y `seq` z `seq` (nextLine (w,x,y,z) a)) (zip seqs $ repeat Map.empty, Vector.empty, 1, []) genome
	--scores are [(Sequence, Map PeakClass (InnerCount, OuterCount))]
	nextLine (scores, cur, index, peaks) next = if (B.head next) == '>'
				then (scores, Vector.empty, 1, getPeaks $ byteStringToString $ B.tail next)
				else processLine scores cur index peaks next

	getPeaks chrom = sort $ map (processPeak) $ fromMaybe [] $ Map.lookup chrom peakMap

	processPeak peak = (s, e, peak) where
		s = max 1 $ (start peak) - ((peakspan peak) `div` 2)
		e = (end peak) + ((peakspan peak) `div` 2)
	
	processLine scores cur index [] next = (scores, Vector.empty, 1, [])
	processLine scores cur index peaks next 
		| (ps <= e) = processLine' scores (cur Vector.++ (byteStringToVector next)) index peaks
		| otherwise = (scores, Vector.empty, e, peaks)
		where
			(ps, pe, p) = head peaks
			e = index + Vector.length cur + (fromIntegral $ B.length next)

	processLine' scores cur index [] = (scores, Vector.empty, 1, [])
	processLine' scores cur index (p:peaks) = if within index endIndex pstart pend
		then deepseq updatedScores $ processLine' updatedScores cur index peaks
		else (scores, cur', index', (p:peaks))
		where
			(pstart, pend, peak) = p
			los = Vector.slice (pstart - index) ((start peak) - pstart) cur
			is = Vector.slice ((start peak) - index) ((end peak) - (start peak) + 1) cur
			ros = Vector.slice ((end peak) - index + 1) (pend - (end peak)) cur
			updatedScores = scorePeak scores los is ros $ classification peak
			endIndex = index + (Vector.length cur) - 1
			index' = min pstart $ endIndex + 1
			cur' = Vector.drop (index' - index) cur

	within s1 e1 s2 e2 = (s1 <= s2) && (e1 >= e2)

	scorePeak scores los is ros cls = map (\(sqnc, m) -> seq m $ (sqnc, updateScore sqnc m los is ros cls)) scores

	updateScore sqnc m los is ros cls = seq count' $ Map.insert cls count' m where
		(innercount, outercount) = fromMaybe (0,0) $ Map.lookup cls m
		count' = seq innercount $ seq outercount $ (innercount + (occurences sqnc is), outercount + (occurences sqnc los) + (occurences sqnc ros))

	occurences s1 s2 = length $ filter (\x-> (matches' s1 x) || (matches' (DNA.rvscmpl s1) x)) $ map (\x-> Vector.slice x (length s1) s2) [0..((Vector.length s2) - (length s1))]

	matches' s v
		| s == [] && Vector.null v = True
		| s == [] || Vector.null v = False
		| ((Char.toLower $ head s) /= (Vector.head v)) && ((Char.toUpper $ head s) /= (Vector.head v)) = False
		| otherwise = matches' (tail s) (Vector.tail v)


type Genome = B.ByteString
type Sequence = (Vector.Vector Char, Vector.Vector Char)
type Enrichment = [(Vector.Vector Char, (Int, Int))]
newtype Reading = Reading Int deriving (Eq, Ord)
(chromName,chromSeq) = (Reading 1, Reading 2)
type Accumulator = (Reading, Int, Int, [Char], [PeakRegion], [(Peak, Enrichment)])
type PeakRegion = (Int, Int, Peak)

seqenrichment :: [(Peak, Enrichment)] -> [(String, Map.Map String (Int, Int))]
seqenrichment [] = []
seqenrichment peakE = calc peakE Map.empty where
	calc [] coremap = Map.toList coremap
	calc ((p,e):xs) coremap = calc xs $ foldl' (\m (core,(i,o))-> updateCore (Vector.toList core) (cls, (i,o)) m) coremap e where
		cls = classification p
	updateCore k (cls, (x',y')) m = Map.insert k m' m where m' = updateCls cls (x',y') $ fromMaybe Map.empty $ Map.lookup k m
	updateCls k (x',y') m = Map.insert k (x'+x, y'+y) m where (x,y) = fromMaybe (0,0) $ Map.lookup k m

enrichmentalt :: Genome -> [Sequence] -> Map.Map String [Peak] -> [(Peak, Enrichment)]
--process the genome character by character
enrichmentalt g seqs peakmap = enrichment where
	emptyAccumulator = (chromSeq, 1, 1, [], [], [])
	(_, _, _, _, _, enrichment) = B.foldl' (processChar) (emptyAccumulator) g

	--take the current accumulated state, process the new character, and return the new state
	processChar :: Accumulator -> Char -> Accumulator
	processChar (state, si, ei, str, peakregions, enrichment) c
		| state == chromName = processChromName (state, si, ei, str, peakregions, enrichment) c
		| state == chromSeq = processChromSeq (state, si, ei, str, peakregions, enrichment) c

	processChromName :: Accumulator -> Char -> Accumulator
	{-# INLINE processChromName #-}
	processChromName (_, si, ei, str, peakregions, enrichment) '\n' = (chromSeq, 1, 1, [], getPeakRegions $ reverse str, enrichment)
	processChromName (_, si, ei, str, peakregions, enrichment) c = (chromName, si, ei, c:str, peakregions, enrichment)

	getPeakRegions :: [Char] -> [PeakRegion]
	{-# INLINE getPeakRegions #-}
	getPeakRegions str = sort $ filter (\(s,e,p)-> s > 0) $ map (\p-> ((start p) - ceiling((fromIntegral $ peakspan p)/2), (end p) + ceiling((fromIntegral $ peakspan p)/2) , p)) $ fromMaybe [] $ Map.lookup str peakmap

	processChromSeq :: Accumulator -> Char -> Accumulator
	{-# INLINE processChromSeq #-}
	processChromSeq accum '\n' = accum
	processChromSeq (_, _, _, _, _, enrichment) '>' = (chromName, 1, 1, [], [], enrichment)
	processChromSeq (_, si, ei, str, [], enrichment) c = (chromSeq, 1, 1, [], [], enrichment)
	processChromSeq (_, si, ei, str, (pr:peakregions), enrichment) c
		| s > ei = (chromSeq, ei+1, ei+1, [], (pr:peakregions), enrichment)
		| otherwise = processChromSeq' si (ei+1) (c:str) (pr:peakregions) enrichment
		where (s, _, _) = pr
	
	processChromSeq' :: Int -> Int -> [Char] -> [PeakRegion] -> [(Peak,Enrichment)] -> Accumulator
	{-# INLINE processChromSeq' #-}
	processChromSeq' si ei str [] enrichment = (chromSeq, si, ei, str, [], enrichment)
	processChromSeq' si ei str (pr:peakregions) !enrichment
		| e > ei = (chromSeq, si, ei, str, (pr:peakregions), enrichment)
		| otherwise = processChromSeq' si ei str peakregions ((p, processPeakRegion pr $ reverse $ take (e - s + 1) str):enrichment)
		where (s, e, p) = pr

	processPeakRegion :: PeakRegion -> [Char] -> Enrichment
	{-# INLINE processPeakRegion #-}
	processPeakRegion (s, e, p) str = map (\(x,y)-> (x, (innerCount (x,y), outerCount (x,y)))) seqs where
		innerCount sqnc = count' sqnc peakseq
		outerCount sqnc = (count' sqnc lflank) + (count' sqnc rflank)
		lflank = take ((start p) - s) str
		rflank = drop ((end p) - e + 1) str
		peakseq = take (peakspan p) $ drop ((start p) - s) str

	count' :: Sequence -> [Char] -> Int
	{-# INLINE count' #-}
	count' (fwd, rvs) str = count'' fwd rvs 0 str where
		count'' xs ys !num [] = num
		count'' xs ys !num zs = if (prefix' xs zs) || (prefix' ys zs) then count'' xs ys (num+1) (tail zs) else count'' xs ys num (tail zs)
		prefix' xs zs
			| (Vector.null xs) && (zs == []) = True
			| Vector.null xs = False
			| zs == [] = False
			| (Vector.head xs) /= (head zs) = False
			| otherwise = prefix' (Vector.tail xs) (tail zs)

--returns a mapping of chromosome to all peaks on that chromosome
parsePeaks :: String -> String -> Map.Map String [Peak]
parsePeaks classification file = parse' (lines file) Map.empty where
	parse' [] m = m
	parse' (x:xs) m = parse' xs m' where
		p = parse'' $ words x
		chrom = chromosome p
		m' = Map.insert chrom (p:(fromMaybe [] $ Map.lookup chrom m)) m
	parse'' (chrom:start:end:_) = (classification, chrom, read start, read end)

classify :: Double -> [Map.Map String [Peak]] -> Map.Map String [Peak]
classify cutoff [] = Map.empty
classify cutoff [x] = x
--classify xs = foldl combine Map.empty xs where
classify cutoff xs = classify' (foldl combine Map.empty xs) xs where

	classify' :: Map.Map String [Peak] -> [Map.Map String [Peak]] -> Map.Map String [Peak]
	classify' m [] = m
	classify' m (y:ys) = classify' m' ys' where
		(m', ys') = classify'' m y ys

	classify'' :: Map.Map String [Peak] -> Map.Map String [Peak] -> [Map.Map String [Peak]] -> (Map.Map String [Peak], [Map.Map String [Peak]])
	classify'' m y ys = (m', ys') where
		(m', ys') = Map.fold (\a (m1, zs)-> foldl (\(m2, ws) p-> classify''' m2 p ws) (m1,zs) a) (m, ys) y

	classify''' :: Map.Map String [Peak] -> Peak -> [Map.Map String [Peak]] -> (Map.Map String [Peak], [Map.Map String [Peak]])
	classify''' m p ys = (m', ys') where
		overlap = concatMap (overlapPeaks cutoff p) ys
		ys' = map (removeAll overlap) ys
		m' = foldl (flip Main.insert) m $ joinPeaks (p:overlap)

classifyalt :: Int -> Map.Map String [Peak] -> Map.Map String [Peak]
classifyalt dist peaks = Map.map (classifyalt') peaks where
	classifyalt' [] = []
	classifyalt' (p:ps) = p'++(classifyalt' ps') where
		(p', ps') = classifyalt'' (start p, end p, [p]) ps
	classifyalt'' (s,e,ps) ops
		| ps' == [] = (joinPeaks ps, ops)
		| otherwise = classifyalt'' (s',e',ps++ps') ops'
		where
			(ps', ops') = partition (\x-> (distance ([],[],s,e) x) <= dist) ops
			(s', e') = foldl (\(a,b) y -> combinedSpan ([],[],a,b) y) (s,e) ps'

insert :: Peak -> Map.Map String [Peak] -> Map.Map String [Peak]
insert p m = Map.insert (chromosome p) (p:(fromMaybe [] $ Map.lookup (chromosome p) m)) m

removeAll :: [Peak] -> Map.Map String [Peak] -> Map.Map String [Peak]
removeAll ps m = foldl (flip remove) m ps

remove :: Peak -> Map.Map String [Peak] -> Map.Map String [Peak]
remove p m = Map.adjust (remove' p) (chromosome p) m where
	remove' :: Peak -> [Peak] -> [Peak]
	remove' p [] = []
	remove' p (x:xs) = if p == x then remove' p xs else x:(remove' p xs)

expandPeakMap :: Map.Map String [Peak] -> [Peak]
expandPeakMap m = Map.fold (\x y-> x ++ y) [] m

joinPeaks :: [Peak] -> [Peak]
joinPeaks [] = error "No peaks to join"
joinPeaks [(desc, chr, s, e)] = [(desc ++ "_Unique", chr, s, e)]
joinPeaks xs = if length cats <= 1 then [(desc, chr, s, e)] else (desc, chr, s, e):(map (\str-> (str++"_NonUnique",chr,s,e)) cats) where
	(_, chr, s, e) = joinPeaks' xs
	cats = nub $ sort $ map (\(d,_,_,_) -> d) xs
	desc = if (length cats) == 1 then (head cats) ++ "_Unique" else tail $ foldl (\x y-> x ++ "_" ++ y) "" cats
	joinPeaks' [p] = p
	joinPeaks' (p:ps) = join' p $ joinPeaks' ps where
		join' (_, aChr, aSt, aEnd) (_, bChr, bSt, bEnd) = ("", aChr, nStart, nEnd) where
			(nStart, nEnd) = combinedSpan ([],[],aSt,aEnd) ([],[],bSt,bEnd)

combinedSpan :: Peak -> Peak -> (Int,Int)
combinedSpan (_,_,s1,e1) (_,_,s2,e2) = (min s1 s2, max e1 e2)


distance :: Peak -> Peak -> Int
distance (_,_,s1,e1) (_,_,s2,e2)
	| s1 < s2 = s2 - e1
	| s2 < s1 = s1 - e2
	| otherwise = max (s2-e1) (s1-e2) 
	

overlapPeaks:: Double -> Peak -> Map.Map String [Peak] -> [Peak]
overlapPeaks d p m = filter (overlaps d p) chromPeaks where
	chromPeaks = fromMaybe [] $ Map.lookup (chromosome p) m

overlaps :: Double -> Peak -> Peak -> Bool
overlaps d (_, _, as, ae) (_, _, bs, be) = (((fromIntegral overlap) / (fromIntegral peakspan)) > d) where
	overlap
		| (as > bs) && (bs >= ae) = bs - ae + 1
		| (bs > as) && (as >= be) = as - be + 1
		| bs == as = min (as - ae + 1) (bs - be + 1)
		| otherwise = 0
	peakspan
		| as > bs = as - be + 1
		| bs > as = bs - ae + 1
		| otherwise = max (as - ae + 1) (bs - be + 1)

combine :: Map.Map String [Peak] -> Map.Map String [Peak] -> Map.Map String [Peak]
combine x y = foldl insert' Map.empty keys where
	keys = nub $ sort $ (Map.keys x) ++ (Map.keys y)
	combine' k = (fromMaybe [] $ Map.lookup k x) ++ (fromMaybe [] $ Map.lookup k y)
	insert' m k = Map.insert k (combine' k) m
