--takes the extracted names, sequences, and intensities produced by extract.hs and removes the primer sequence
--of specified length from the sequences

import System.Environment

trimprimer :: Int -> String -> String
trimprimer len line = unwords [name, take (length sqnc - len) sqnc, intensity]
	where [name, sqnc, intensity] = words line

main = do
	args <- getArgs
	let primer = if length args >= 1 then read $ head args else error "Primer length must be passed as an argument."
	interact (\x-> unlines $ map (trimprimer primer) $ lines x)