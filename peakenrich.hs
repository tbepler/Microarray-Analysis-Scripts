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

chromosome :: Peak -> String
chromosome (_, c, _, _) = c

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
classify xs = classify' Map.empty xs where
	classify' m [] = m
	classify' m (x:xs) = 

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
	let peakmap =  $ map (parsePeaks) peakfiles