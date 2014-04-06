--performs regression analysis on probes grouped by the cores given

--author: Tristan Bepler (tbepler@gmail.com)

import qualified Table as Table
import qualified Data.Vector.Unboxed as Vector
import qualified Utils as Util
import qualified DNA as DNA
import Data.Maybe ( fromMaybe )
import Data.List
import Data.Typeable
import Statistics.LinearRegression
import Statistics.Types
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
	(Table.StringE sqnc) = fromMaybe (Table.StringE "") $ Table.lookupEntry "Sequence" row
	core' [] s = ""
	core' (x:xs) s = if coreMatches x s then x else core' xs s

regressionAnalysis :: (String, [Table.Row]) -> Table.Row
regressionAnalysis (coreSeq, rows) = Table.Row ( [("Sequence", Table.StringE coreSeq), ("Name", Table.StringE ("Core" ++ (show $ length coreSeq))), ("Count", Table.IntE $ length rows)] ++ (analysis vects) ) where
	cols = filter (\x-> (Table.columnType x) == (typeOf ([1.0] :: [Double]))) $ Table.rowsToCols rows
	vects = map (\x-> (Table.name x, Vector.fromList $ Table.doubleEntries x)) cols
	analysis [] = []
	analysis [x] = []
	analysis (x:xs) = (analysis' x xs) ++ analysis xs 
	analysis' x [] = []
	analysis' x (y:ys) = (analysis'' x y):(analysis' x ys)
	analysis'' (s1, x) (s2, y) = (s1++"_"++s2, Table.DoubleE $ (correl x y)^2)


main = do
	[Cores coresfilepath, Probes probesfilepath] <- getArgs >>= parse
	coresfile <- readFile coresfilepath
	probesfile <- readFile probesfilepath
	let cores = Table.stringEntries $ fromMaybe (Table.StringCol "Sequence" []) $ Table.lookupCol "Sequence" $ Table.columns $ read coresfile
	let probeGroups = filter (\(c, _) -> c /= "") $ Util.groupBy (probeCore cores) $ Table.rows $ read probesfile
	putStrLn $ show $ Table.FromRows $ map (regressionAnalysis) probeGroups