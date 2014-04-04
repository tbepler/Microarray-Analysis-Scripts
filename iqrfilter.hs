--filters files for rows with IQR column values exceeding the specified cuttoff

--author: Tristan Bepler (tbepler@gmail.com)

import System.Environment
import Data.List
import qualified Table as Table

iqrfilter :: Double -> [Table.Row] -> [Table.Row]
iqrfilter cutoff rows = filter (meetsCriteria') rows where
	meetsCriteria' (Table.Row xs) = or $ map (meetsCriteria'') xs
	meetsCriteria'' (name, Table.DoubleE d) = (isSuffixOf "IQR" name) && d >= cutoff 
	meetsCriteria'' (name, _) = False

main = do
	args <- getArgs
	let cutoff = if length args >= 1 then read $ head args else error "Must specify a cutoff value"
	interact ( show . Table.FromRows . iqrfilter cutoff . readRows . lines) where
		readRows (header:body) = map (Table.parseRow $ words header) body
