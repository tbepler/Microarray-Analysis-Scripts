--performs regression analysis on probes grouped by the cores given

--author: Tristan Bepler (tbepler@gmail.com)

import qualified Table as Table
import qualified Data.Vector as Vector
import qualified Utils as Util
import qualified DNA as DNA
import Data.Maybe ( fromMaybe )
import Data.List
import Statistics.LinearRegression
import System.Environment
import System.Exit
import System.IO
import System.Console.GetOpt

data Flag
	= Cores String		--cores -c
	| Probes String		--probes -p
	| Help				--help -h
	deriving (Eq, Ord, Show)

flags = 
	[ 
	Option ['c'] ["cores"] (ReqArg Cores "FILE") "File containing cores to group probes by",
	Option ['p'] ["probes"] (ReqArg Probes "FILE") "Probes file",
	Option ['h'] ["help"] (NoArg Help) "Print this help message"
	]

parse argv = case getOpt Permute flags argv of
	([], _, []) -> do 
					hPutStrLn stderr (usageInfo header flags)
					exitWith ExitSuccess
	(args, _ , []) -> if elem Help args
				then do 
					hPutStrLn stderr (usageInfo header flags)
					exitWith ExitSuccess
				else return $ nub $ sort args
	(_,_,errs) -> do
			hPutStrLn stderr (concat errs ++ usageInfo header flags)
			exitWith (ExitFailure 1)
	where header = "Usage: coreregression -c/--cores=FILE -p/--probes=FILE"

core :: Int -> String -> String
core n sqnc = drop flank $ take (flank + n) sqnc
	where flank = ((length sqnc) - n) `div` 2

coreMatches :: String -> String -> Bool
coreMatches x sqnc = (x == y) || ((DNA.rvscmpl x) == y) where y = core (length x) sqnc

probeCore :: [String] -> Table.Row -> String
probeCore cores row = core' cores sqnc where
	(Table.SequenceE sqnc) = Table.lookupEntry "Sequence" row
	core' [] s = ""
	core' (x:xs) s = if coreMatches x s then x else core' xs s

regressionAnalysis :: (String, [Table.Row]) -> Table.Row


main = do
	[Cores coresfilepath, Probes probesfilepath] <- getArgs >>= parse
	coresfile <- readFile coresfilepath
	probesfile <- readFile probesfilepath
	let cores = Table.stringEntries $ fromMaybe (Table.StringCol "Sequence" []) $ Table.lookupCol "Sequence" $ Table.columns $ read coresfile
	let probeGroups = filter (\(c, _) -> c /= "") $ Util.groupBy (probeCore cores) $ Table.rows $ read probesfile
	putStrLn $ show cores