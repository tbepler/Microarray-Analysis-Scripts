--this script takes peak files, sequences, and the genome and calculates the enrichment of the given
--sequences in the given peaks.

--author: Tristan Bepler (tbepler@gmail.com)

--Usage: peakenrich -s/--seqs=FILE -g/--genome=FILE NAME=PEAK_FILE...

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Data.Maybe (fromMaybe)
import Data.List
import Control.DeepSeq
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import qualified Data.Char as Char
import qualified Table as Table
import qualified Utils as Util
import qualified DNA as DNA

import Debug.Trace

data Flag
	= Help
	| Seqs String
	| Genome String
	| Overlap Double
	deriving (Eq, Ord, Show)

flags = 
	[ 
	Option ['s'] ["seqs"] (ReqArg Seqs "FILE") "File containing sequences to calculate enrichment of",
	Option ['g'] ["genome"] (ReqArg Genome "FILE") "Genome file in fasta format",
	Option ['o'] ["overlap"] (OptArg parseOverlap "RATIO") "Cutoff for ratio of bases that need to be shared for peaks to be considered overlapping. Default = 0.0",
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
					cutoff [] = 0.0
					cutoff [Overlap d] = d
					cutoff _ = 0.0
	(_,_,errs) -> do
			hPutStrLn stderr (concat errs ++ usageInfo header flags)
			exitWith (ExitFailure 1)
	where header = "Usage: peakenrich -s/--seqs=FILE -g/--genome=FILE NAME=PEAK_FILE..."

parseOverlap :: Maybe String -> Flag
parseOverlap (Nothing) = Overlap 0.0
parseOverlap (Just s) = Overlap $ read s

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
	--read the genome lazily as a bytestring
	genomefile <- BL.readFile genomefilepath
	let genome = BL.lines genomefile
	--read in the peaks, classify them, and store them in a map by chromosome
	let (peaknames, peakfilepaths) = unzip $ map (parsePeakArg) peakargs
	peakfiles <- mapM (readFile) peakfilepaths
	let peakmap = classify cutoff $ map (\(x,y)-> parsePeaks x y) $ zip peaknames peakfiles
	--putStrLn $ unlines $ map (\(s, xs)-> unwords (s:(map (show) xs))) $ Util.groupBy classification $ expandPeakMap peakmap
	let (cls, count) = unzip $ map (\(s, xs) -> (s, show $ length xs)) $ Util.groupBy classification $ expandPeakMap peakmap
	putStrLn $ unlines $ (unwords cls):[(unwords count)]
	putStrLn $ showEnrichmentScores $ enrichment genome seqs peakmap

	

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

byteStringToString :: BL.ByteString -> String
byteStringToString bs
	| BL.null bs = ""
	| otherwise = (BL.head bs):(byteStringToString $ BL.tail bs)

byteStringToVector :: BL.ByteString -> Vector.Vector Char
byteStringToVector bs
	| BL.null bs = Vector.empty
	| otherwise = Vector.cons (BL.head bs) (byteStringToVector $ BL.tail bs)

instance (NFData k, NFData a) => NFData (Map.Map k a) where
	 rnf = rnf . Map.toList

--takes the genome, sequences, and peaks and computes the enrichment of each sequence for each peak
enrichment :: [BL.ByteString] -> [String] -> Map.Map String [Peak] -> [(String, Map.Map String (Int, Int))]
enrichment genome seqs peakMap = scores' where
	(scores', _, _, _) = foldl' (\(w,x,y,z) a-> w `seq` x `seq` y `seq` z `seq` (nextLine (w,x,y,z) a)) (zip seqs $ repeat Map.empty, Vector.empty, 1, []) genome
	--scores are [(Sequence, Map PeakClass (InnerCount, OuterCount))]
	nextLine (scores, cur, index, peaks) next = if (BL.head next) == '>'
				then (scores, Vector.empty, 1, getPeaks $ byteStringToString $ BL.tail next)
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
			e = index + Vector.length cur + (fromIntegral $ BL.length next)

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
		m' = Main.insert (joinPeaks (p:overlap)) m

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

joinPeaks :: [Peak] -> Peak
joinPeaks [] = error "No peaks to join"
joinPeaks [(desc, chr, s, e)] = (desc ++ "_Unique", chr, s, e)
joinPeaks xs = (desc, chr, s, e) where
	(_, chr, s, e) = joinPeaks' xs
	desc = tail $ foldl (\x y-> x ++ "_" ++ y) "" $ nub $ sort $ map (\(d, _, _, _) -> d) xs
	joinPeaks' [p] = p
	joinPeaks' (p:ps) = join' p $ joinPeaks' ps where
		join' (_, aChr, aSt, aEnd) (_, bChr, bSt, bEnd) = ("", aChr, nStart, nEnd) where
			nStart = if aSt > bSt then aSt else bSt
			nEnd = if aEnd < bEnd then aEnd else bEnd

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
