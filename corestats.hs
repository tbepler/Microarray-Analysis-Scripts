--this script groups probes by cores of the given length and reports some statistics
--on those core groups for each TF column given

--usage: corestats -core=core_size < probe matrix > core statistics

--author: Tristan Bepler (tbepler@gmail.com)

import System.Environment
import qualified Data.Map as Map
import qualified Utils as Util
import qualified Probe as Probe

computeStats :: [(String, [Probe.Probe])] -> [Probe.Probe]
computeStats probeGroups = map (coreStats) probeGroups
 
coreStats :: (String, [Probe.Probe]) -> Probe.Probe
coreStats (core, []) = Probe.Probe name core [] where
	name = "Core" ++ (show $ length core)
coreStats (core, probes) = Probe.Probe name core stats where
	name = "Core" ++ (show $ length core)
	stats = ("Count", fromIntegral $ length probes):(concatMap (colStats) cols)
	cols = valueCols probes
	colStats (name, xs) = [(name++"_Median", Util.median xs), (name++"_IQR", Util.iqr xs), (name++"_Range", Util.range xs)]

valueCols :: [Probe.Probe] -> [(String, [Double])]
valueCols [] = []
valueCols [p] = map (\(x, y)->(x, [y])) $ Probe.values p
valueCols (p:ps) = map (\((n, x), (m, y))-> (n, x:y)) $ zip (Probe.values p) (valueCols ps)


main = do
	args <- getArgs
	let coresize = if length args > 0 then read $ head args else error "Must specify core size"
	interact (Probe.toString . computeStats . (Probe.groupByCore coresize) . readInput . lines) where
		readInput (header:body) = Probe.readProbes header body
