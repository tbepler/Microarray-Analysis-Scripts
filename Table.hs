--this module provides definition of a table data type

--The table can be used to represent data where each column is homogenously composed of either
--Int, Double, or String and rows are heterogenous compositions of those three types.
--Columns may have headers and are accessible through those names.

--author: Tristan Bepler (tbepler@gmail.com)

{-# LANGUAGE DeriveDataTypeable #-}

module Table where

import Data.Typeable
import Data.Char as Char

data Table = FromCols [Column] | FromRows [Row] deriving (Eq) 

instance Show Table where
	show (FromCols cols) = unlines $ (unwords $ colnames (FromCols cols)):(map (rowToString) $ colsToRows cols)
	show (FromRows rows) = unlines $ (unwords $ colnames (FromRows rows)):(map (rowToString) rows)
	
instance Read Table where
	readsPrec _ s = [(FromRows $ map (parseRow $ words header) body , "")] where
		(header:body) = lines s

columns :: Table -> [Column]
columns (FromCols cols) = cols
columns (FromRows rows) = rowsToCols rows

rows :: Table -> [Row]
rows (FromCols cols) = colsToRows cols
rows (FromRows rows) = rows

colnames :: Table -> [String]
colnames (FromCols cols) = map (name) cols
colnames (FromRows ((Row x):_)) = names where
	(names, _) = unzip x

colsToRows :: [Column] -> [Row]
colsToRows [] = []
colsToRows cols = foldr (combine') [] cols where
	combine' col [] = colToRows col
	combine' col rows = map (\(x,y) -> concatRows x y) $ zip (colToRows col) rows

rowsToCols :: [Row] -> [Column]
rowsToCols [] = []
rowsToCols rows = foldr (combine') [] rows where
	combine' row [] = rowToCols row
	combine' row cols = map (\(x,y)-> concatCols x y) $ zip (rowToCols row) cols

data Column = IntCol {name :: String, intEntries :: [Int]} | DoubleCol {name :: String, doubleEntries :: [Double]} | StringCol {name :: String, stringEntries :: [String]} deriving (Eq, Ord)

columnType :: Column -> TypeRep
columnType (IntCol _ xs) = typeOf xs
columnType (DoubleCol _ xs) = typeOf xs
columnType (StringCol _ xs) = typeOf xs

concatCols :: Column -> Column -> Column
concatCols (IntCol n xs) (IntCol _ ys) = IntCol n (xs++ys)
concatCols (DoubleCol n xs) (DoubleCol _ ys) = DoubleCol n (xs++ys)
concatCols (StringCol n xs) (StringCol _ ys) = StringCol n (xs++ys)
concatCols _ _ = error "Columns must be of same type and same name to be concatenated"

colToRows :: Column -> [Row]
colToRows (IntCol n xs) = map(\x-> Row [(n, IntE x)]) xs
colToRows (DoubleCol n xs) = map(\x-> Row [(n, DoubleE x)]) xs
colToRows (StringCol n xs) = map(\x-> Row [(n, StringE x)]) xs

data Row = Row [(String, Entry)] deriving (Eq, Ord)

concatRows :: Row -> Row -> Row
concatRows (Row a) (Row b) = Row (a ++ b)

rowToCols :: Row -> [Column]
rowToCols (Row xs) = map (unentry) xs where
	unentry (name, IntE i) = IntCol name [i]
	unentry (name, DoubleE d) = DoubleCol name [d]
	unentry (name, StringE s) = StringCol name [s]

rowToString :: Row -> String
rowToString (Row xs) = unwords $ map (\(n,e)-> show e) xs

parseRow :: [String] -> String -> Row
parseRow [] s = Row $ zip (map (show) [1..]) (map (read) $ words s)
parseRow header s = Row $ zip header $ map (read) $ words s

rowEntries :: Row -> [Entry]
rowEntries (Row xs) = entries where
	(names, entries) = unzip xs
	

data Entry = IntE Int | DoubleE Double | StringE String deriving (Eq, Ord, Typeable)

instance Show Entry where
	show (IntE i) = show i
	show (DoubleE d) = show d
	show (StringE s) = s

instance Read Entry where
	readsPrec _ s 
		| (isInt s) = [(IntE (read s), "")]
		| (isDouble s) = [(DoubleE (read s), "")]
		| otherwise = [(StringE s, "")]
		where
			isInt s = case reads s :: [(Int, String)] of
  				[(_, "")] -> True
  				_         -> False
			isDouble s = case reads s :: [(Double, String)] of
  				[(_, "")] -> True
  				_         -> False

entryType :: Entry -> TypeRep
entryType (IntE i) = typeOf i
entryType (DoubleE d) = typeOf d
entryType (StringE s) = typeOf s

