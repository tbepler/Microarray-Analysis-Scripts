--this script takes peak files, sequences, and the genome and calculates the enrichment of the given
--sequences in the given peaks.

--author: Tristan Bepler (tbepler@gmail.com)

--Usage: peakenrich -s/--seqs=FILE -g/--genome=FILE PEAK_FILES...

import System.Console.GetOpt
import System.Environment
import System.IO
import System.Exit
import Data.Maybe (fromMaybe)
import Data.List
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Table as Table
import qualified Utils as Util


data Flag
	= Help
	| Seqs String
	| Genome String
	deriving (Eq, Ord, Show)

flags = 
	[ 
	Option ['s'] ["seqs"] (ReqArg Seqs "FILE") "File containing sequences to calculate enrichment of",
	Option ['g'] ["genome"] (ReqArg Genome "FILE") "Genome file in fasta format",
	Option ['h'] ["help"] (NoArg Help) "Print this help message"
	]

parse argv = case getOpt Permute flags argv of
	([Seqs seqfilepath, Genome genomefilepath], peakfilepaths, []) -> return (seqfilepath, genomefilepath, peakfilepaths)
	([Genome genomefilepath, Seqs seqfilepath], peakfilepaths, []) -> return (seqfilepath, genomefilepath, peakfilepaths)
	(_, _, []) -> do 
			hPutStrLn stderr (usageInfo header flags)
			exitWith ExitSuccess
	(_,_,errs) -> do
			hPutStrLn stderr (concat errs ++ usageInfo header flags)
			exitWith (ExitFailure 1)
	where header = "Usage: peakenrich -s/--seqs=FILE -g/--genome=FILE PEAK_FILES..."

--peak = (classification, chromosome, start, end)
type Peak = (String, String, Int, Int)

classification :: Peak -> String
classification (c, _, _, _) -> c

chromosome :: Peak -> String
chromosome (_, c, _, _) = c

start :: Peak -> Int
start (_, _, s, _) = s

end :: Peak -> Int
end (_, _, _, e) = e

--returns a mapping of chromosome to all peaks on that chromosome
parsePeaks :: String -> String -> Map.Map String [Peak]
parsePeaks file classification = parse' (lines file) Map.empty where
	parse' [] m = m
	parse' (x:xs) m = parse' xs m' where
		p = parse'' $ words x
		chrom = chromosome p
		m' = Map.insert chrom (p:(fromMaybe [] $ Map.lookup chrom m)) m
	parse'' (chrom:start:end:_) = (classification, chrom, read start, read end)

classify :: [Map.Map String [Peak]] -> Map.Map String [Peak]
classify [] = Map.empty
classify [x] = x
classify xs = classify' (foldl combine Map.empty xs) xs where
	insert p m = Map.insert (chromosome p) (p:(fromMaybe [] $ Map.lookup (chromosome p) m)) m
	removeAll ps m = foldl (flip remove) m ps
	remove p m = Map.adjust (remove' p) (chromosome p) m
	remove' p [] = []
	remove' p (x:xs) = if p == x then remove' p xs else x:(remove' p xs)
	classify' m [] = m
	classify' m (x:xs) = classify' m' xs' where
		(m', xs') = classify'' m x xs
	classify'' m x xs = (m', xs') where
		(m', xs') = Map.fold (\a (m,xs)-> foldl (\(m, xs) p-> classify''' m p xs) (m,xs) a) (m, xs) m
	classify''' m p xs = (m', xs') where
		overlap = concatMap (overlapPeaks p) xs
		xs' = map (removeAll overlap) xs
		m' = insert (joinPeaks (p:overlap)) m

expandPeakMap :: Map.Map String [Peak] -> [Peak]
expandPeakMap m = Map.fold (\x y-> x ++ y) [] m

joinPeaks :: [Peak] -> Peak
joinPeaks [] = error "No peaks to join"
joinPeaks [(desc, chr, s, e)] = (desc ++ "_Unique", chr, s, e)
joinPeaks xs = (desc, chr, s, e) where
	(_, chr, s, e) = joinPeaks' xs
	desc = init $ foldl (\x y-> x ++ "_" ++ y) "" $ nub $ sort $ map (\(d, _, _, _) -> d) xs
	joinPeaks' [p] = p
	joinPeaks' (p:ps) = join' p $ joinPeaks ps where
		join' (_, aChr, aSt, aEnd) (_, bChr, bSt, bEnd) = ("", aChr, nStart, nEnd) where
			nStart = if aSt > bSt then aSt else bSt
			nEnd = if aEnd < bEnd then aEnd else bEnd

overlapPeaks:: Peak -> Map.Map String [Peak] -> [Peak]
overlapPeaks p m = filter (overlaps p) chromPeaks where
	chromPeaks = fromMaybe [] $ Map.lookup (chromosome p) m

overlaps :: Peak -> Peak -> Bool
overlaps (_, _, as, ae) (_, _, bs, be) = (((fromIntegral overlap) / (fromIntegral peakspan)) >= 0.5) where
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


main = do
	(seqfilepath, genomefilepath, peakfilepaths) <- getArgs >>= parse
	--read the seqfile and extract the sequences column from the file
	seqfile <- readFile seqfilepath
	let seqs = Table.stringEntries . fromMaybe (Table.StringCol "Sequence" []) . Table.lookupCol "Sequence" . Table.columns . read seqfile
	--read the genome lazily as a bytestring
	genomefile <- BL.readFile genomefilepath
	let genome = BL.unpack genomefile
	--read in the peaks, classify them, and store them in a map by chromosome
	peakfiles <- map (readFile) peakfilepaths
	let peakmap = classify $ map (parsePeaks) peakfiles
	putStrLn $ unlines $ map (\(s, xs)-> unwords (s:(map (show) xs))) $ Util.groupBy classification $ expandPeakMap peakmap