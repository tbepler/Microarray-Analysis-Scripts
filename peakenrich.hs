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
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as Map
import qualified Table as Table
import qualified Utils as Util


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
	let genome = BL.unpack genomefile
	--read in the peaks, classify them, and store them in a map by chromosome
	let (peaknames, peakfilepaths) = unzip $ map (parsePeakArg) peakargs
	peakfiles <- mapM (readFile) peakfilepaths
	let peakmap = classify cutoff $ map (\(x,y)-> parsePeaks x y) $ zip peaknames peakfiles
	putStrLn $ unlines $ map (\(s, xs)-> unwords (s:(map (show) xs))) $ Util.groupBy classification $ expandPeakMap peakmap

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
