--this script filters a list of probes for probes containing the specified core

--usage: coreselct core1 [core2] ... [coreN] < probes > probes with that core

--author: Tristan Bepler (tbepler@gmail.com)

import System.Environment
import qualified Probe as Probe
import qualified DNA as DNA

coreselect :: [String] -> [Probe.Probe] -> [Probe.Probe]
coreselect cores probes = filter (match') probes where
	match' p = or $ map (contains' p) cores
	contains' p c = ((Probe.core (length c) p) == c) || ((Probe.core (length c) p) == DNA.rvscmpl c) 

main = do
	args <- getArgs
	let cores = if length args > 0 then args else error "Must specify at least one core sequence"
	interact ( Probe.toString . coreselect cores . readProbes . lines) where
		readProbes (header:body) = Probe.readProbes header body 
