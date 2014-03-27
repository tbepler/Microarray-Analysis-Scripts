--extract the names, sequences, and intensities from microarray alldata files
--alldata files in the format:
--Column \t Row \t Name \t ID \t Sequence \t Cy3 \t Cy3Flags \t Alexa488 \t Alexa488Flags \t Cy3Exp \t Obs/Exp \t Alexa488Norm \t Alexa488Median \t Alexa488Adjusted
--Alexa488Adjusted = intensities

import Data.List.Split

--takes indexes of Name, Sequence, and Intensity in the row
--and extracts those features into a new string
extractRow :: Int -> Int -> Int -> String -> String
extractRow name sqnc intensity row = unwords [tokens !! name, tokens !! sqnc, tokens !! intensity]
	where tokens = splitOn "\t" row

--reads the file header and returns the indexes of the name, sequence, and intensity columns
readHeader :: String -> (Int, Int, Int)
readHeader header = (name, sqnc, intensity) 
	where 
		dict = zip (words header) [0..]
		Just name = lookup "Name" dict
		Just sqnc = lookup "Sequence" dict
		Just intensity = lookup "Alexa488Adjusted" dict

--read the header and then extract each row of the input
extract :: [String] -> [String]
extract (header:body) = map (extractRow name sqnc intensity) body
	where (name, sqnc, intensity) = readHeader header

main = interact (\x-> unlines $ extract $ lines x)
