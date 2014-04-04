--test for Table module

import qualified Table as Table
import System.Environment
import Data.Typeable

main = do 
	args <- getArgs
	let
		input :: [Table.Entry]
		input = map (read) args
	putStrLn $ unwords $ map (show) input
	putStrLn $ unwords $ map (show . Table.entryType) input
