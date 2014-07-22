--This script assigns a score to replicate probes by selecting the median score
--of the replicates oriented in the same direction as a given PWM

import qualified Data.Map as Map
import System.Environment
import System.Exit

main = getArgs >>= parse >>= putStrLn

assign (inputFile, pwmFile) = pwm::(Map.Map [Char] [Double]) where
	pwm = readPWM pwmFile

readPWM input = foldl (insert) Map.empty rows where
	rows = extractPWMRows $ lines input
	insert m x = Map.insert (key row) (values row) m where
		row = words x
	key xs = head xs
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
	

parse ["-h"] = help >> exit
parse [x,y] = do
	input <- readFile x
	pwm <- readFile y
	return $ show $ assign (input, pwm)
parse xs = usage >> exit

usage = putStrLn "Usage: orientreps [-h] Reps_Log_File PWM_File"
help = do
	putStrLn "This script takes a combinereps log and assigns a score to replicate probes by selecting the median score of the replicates oriented according to a given PWM."
	usage
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)
