--filters files for rows with IQR column values exceeding the specified cuttoff and counts exceeding the specified cuttoff

--author: Tristan Bepler (tbepler@gmail.com)

import System.Environment
import System.Exit
import System.IO
import Data.List
import System.Console.GetOpt
import qualified Table as Table

data Flag
	= IQR Double 		--iqr -i
	| Count	Int		--count -c
	| Help			--help -h
	deriving (Eq, Ord, Show)

parseIQR :: Maybe String -> Flag
parseIQR (Nothing) = IQR 0
parseIQR (Just d) = IQR $ read d

parseCount :: Maybe String -> Flag
parseCount (Nothing) = Count 0
parseCount (Just n) = Count $ read n

flags = 
	[ 
	Option [] ["i","iqr"] (OptArg (parseIQR) "0") "Filter out cores with all IQRs below specified IQR. Default = 0",
	Option [] ["c","count"] (OptArg (parseCount) "0") "Filter out cores with count less than specified count. Default = 0",
	Option ['h'] ["help"] (NoArg Help) "Print this help message"
	]

parse argv = case getOpt Permute flags argv of
	([], _, []) -> return [IQR 0, Count 0]
	(args, _ , []) -> if elem Help args
				then do hPutStrLn stderr (usageInfo header flags)
					exitWith ExitSuccess
				else return $ nub $ sort args
	(_,_,errs) -> do
			hPutStrLn stderr (concat errs ++ usageInfo header flags)
			exitWith (ExitFailure 1)
	where header = "Usage: corefilter [-i/--iqr=iqr_cutoff] [-c/--count=count_cutoff] < core data"

iqrfilter :: Double -> [Table.Row] -> [Table.Row]
iqrfilter cutoff rows = filter (meetsCriteria') rows where
	meetsCriteria' (Table.Row xs) = or $ map (meetsCriteria'') xs
	meetsCriteria'' (name, Table.DoubleE d) = (isSuffixOf "IQR" name) && d >= cutoff 
	meetsCriteria'' (name, _) = False

countfilter :: Int -> [Table.Row] -> [Table.Row]
countfilter cutoff rows = filter (\x-> (count x) >= cutoff) rows where
	count (Table.Row xs) = extractCount n where (Just n) = if lookup "Count" xs /= Nothing then lookup "Count" xs else Just (Table.DoubleE (1/0))
	extractCount (Table.IntE i) = i
	extractCount (Table.DoubleE d) = ceiling d

main = do
	[(IQR i), (Count c)] <- getArgs >>= parse
	interact ( show . Table.FromRows . countfilter c . iqrfilter i . readRows . lines) where
		readRows (header:body) = map (Table.parseRow $ words header) body
