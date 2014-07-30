--This script orients sequences according to a PWM. The sequences must
--be in the first column of the input file

--Author: Tristan Bepler (tbepler@gmail.com)

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

usage = putStrLn "Usage: orientseqs [-h] Seqs_File PWM_File"
help = do
	putStrLn "This script takes a file containing sequences in the first column and orients them according to the given PWM.\nThe input file should not have a header.\n"
	usage
exit = exitWith ExitSuccess
die = exitWith (ExitFailure 1)

assign (inputFile, pwmFile) = unlines $ filter (not.isEmpty) $ map (process pwm) $ lines inputFile where
	pwm::(Map.Map Char [Double])
	pwm = readPWM pwmFile

isEmpty [] = True
isEmpty x = False

process pwm row = unwords $ (str orientation):rest where
	entries = words row
	rest = tail $ entries
	orientation = orient pwm $ head $ entries
	str Fwd = head $ entries
	str Rvs = rvsCompliment $ head $ entries

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
	score = scoreCenter pwm str
	rvsScore = scoreCenter pwm rvscomp
	rvscomp = rvsCompliment str

scoreCenter pwm str = score pwm center where
	center = take pwmLen $ drop flank str
	flank = (strLen - pwmLen) `div` 2
	strLen = length str 
	pwmLen = pwmLength pwm

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
	


