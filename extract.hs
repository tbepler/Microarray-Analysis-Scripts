--extract the names, sequences, and intensities from microarray alldata files and filters out rows without sequences
--trims primer of specified size from the sequence
--alldata files in the format:
--Column \t Row \t Name \t ID \t Sequence \t Cy3 \t Cy3Flags \t Alexa488 \t Alexa488Flags \t Cy3Exp \t Obs/Exp \t Alexa488Norm \t Alexa488Median \t Alexa488Adjusted
--Alexa488Adjusted = intensities

--usage: extract [primer_length] [--header] < microarray alldata file
--The --header flag is necessary when the input file has a header line
--outputs in the format: name \t trimmed sequence \t intensity

--author: Tristan Bepler (tbepler@gmail.com)

import Data.List.Split
import System.Environment

--takes indexes of Name, Sequence, and Intensity in the row
--and extracts those features into a new string
extractRow :: Int -> Int -> Int -> String -> String
extractRow name sqnc intensity row = unwords [tokens !! name, tokens !! sqnc, roundIntensity $ tokens !! intensity]
	where tokens = splitOn "\t" row

roundIntensity :: String -> String
roundIntensity inten = show ((fromIntegral $ round (read inten * 100)) / 100.0)

--reads the file header and returns the indexes of the name, sequence, and intensity columns
readHeader :: String -> (Int, Int, Int)
readHeader header = (name, sqnc, intensity) 
	where 
		dict = zip (words header) [0..]
		Just name = lookup "Name" dict
		Just sqnc = lookup "Sequence" dict
		Just intensity = lookup "Alexa488Adjusted" dict

--takes the line and trims the suffix of specified length off the sequence
trimPrimer :: Int -> String -> String
trimPrimer len line = unwords [name, take (length sqnc - len) sqnc, intensity]
	where [name, sqnc, intensity] = words line

--read the header and then extract each row of the input
extract :: Bool -> Int -> [String] -> [String]
extract True primer (header:body) = map (trimPrimer primer) $ filter (hasSeq) $ map (extractRow name sqnc intensity) body
	where
		(name, sqnc, intensity) = readHeader header
		hasSeq x = 3 == (length $ words x)
extract False primer body = map (trimPrimer primer) $ filter (hasSeq) $ map (extractRow name sqnc intensity) body
	where
		(name, sqnc, intensity) = (2,4,13)
		hasSeq x = 3 == (length $ words x)


main = do
	args <- getArgs
	let
		primer = if length args >= 1 then read $ head args else 0
		header = any (\x->x=="--header") args
	interact (\x-> unlines $ extract header primer $ lines x)
