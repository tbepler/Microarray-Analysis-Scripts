--This script assigns a score to replicate probes by selecting the median score
--of the replicates oriented in the same direction as a given PWM

import qualified Data.Map as Map
import System.Environment
import System.Exit
import Debug.Trace

data Orientation = Fwd | Rvs

main = getArgs >>= parse >>= putStr

parse ["-h"] = help >> exit
parse [x,y] = do
	input <- readFile x
	pwm <- readFile y
	return $ assign (input, pwm)
parse xs = usage >> exit

usage = putStrLn "Usage: orientreps [-h] Reps_Log_File PWM_File"
help = do
	putStrLn "This script takes a combinereps log and assigns a score to replicate probes by selecting the median score of the replicates oriented according to a given PWM."
	usage
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

assign (inputFile, pwmFile) = unlines $ filter (not.isEmpty) $ map (selectScore pwm) $ {- remove the file header -} tail $ lines inputFile where
	pwm::(Map.Map Char [Double])
	pwm = readPWM pwmFile

isEmpty [] = True
isEmpty x = False

selectScore pwm [] = []
selectScore pwm row = unwords [str,name,(orientAsString orientation),score] where
	entries = words row
	str = head entries
	name = entries !! 1
	score = entries !! (index orientation)
	orientation = orient pwm str 
	
index Fwd = 3
index Rvs = 4

orientAsString Fwd = "Fwd"
orientAsString Rvs = "Rvs"

orient pwm str = if score < rvsScore then Rvs else Fwd where
	score = maxScore pwm str
	rvsScore = maxScore pwm rvscomp
	rvscomp = rvsCompliment str

maxScore pwm str = maximum $ map (score pwm) substrs where
	substrs = map ((take len).flip drop str) [0..((length str)-len)]
	len = pwmLength pwm

rvsCompliment str = reverse $ map (comp) str where
	comp 'A' = 'T'
	comp 'T' = 'A'
	comp 'G' = 'C'
	comp 'C' = 'G'
	comp x = error ("Unknown base: " ++ [x])

pwmLength pwm = minimum $ map (length) $ Map.elems pwm

score pwm str = foldl (add') 0 $ zip [0..] str where
	add' s t = (score' t) + s
	score' (i,c) = score'' i $ Map.lookup c pwm where
		score'' i (Just xs) = xs !! i
		score'' i (Nothing) = error ("Character "++[c]++" not scorable by PWM: " ++ (show pwm))

readPWM input = foldl (insert) Map.empty rows where
	rows = extractPWMRows $ lines input
	insert m x = Map.insert (key row) (values row) m where
		row = words x
	key xs = head $ head xs
	values xs = map (read) $ tail xs

extractPWMRows (x:xs) = if matches row then map (unwords.extract.words) (take 4 (x:xs)) else extractPWMRows xs where
	row = words x
	matches ("A:":ys) = True
	matches ("C:":ys) = True
	matches ("G:":ys) = True
	matches ("T:":ys) = True
	matches ys = False
	extract ("A:":ys) = "A" : ys
	extract ("C:":ys) = "C" : ys
	extract ("G:":ys) = "G" : ys
	extract ("T:":ys) = "T" : ys
	extract ys = ys
	


