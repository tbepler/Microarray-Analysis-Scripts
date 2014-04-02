--this script groups probes by cores of the given length and reports some statistics
--on those core groups for each TF column given

--usage: corestats -core=core_size < probe matrix > core statistics

--author: Tristan Bepler (tbepler@gmail.com)

import System.Environment
import qualified Utils as Util

main = do
	args <- getArgs
	let coresize = if length args > 0 then read $ head args else error "Must specify core size"
	
