--this module provides definition of a table data type

--The table can be used to represent data where each column is homogenously composed of either
--Int, Double, or String and rows are heterogenous compositions of those three types.
--Columns may have headers and are accessible through those names.

--author: Tristan Bepler (tbepler@gmail.com)

{-# LANGUAGE DeriveDataTypeable #-}

module Table where

import Data.Typeable
import Data.Char as Char

data Table = Table 

data Column = IntCol {name :: String, intEntries :: [Int]} | DoubleCol {name :: String, doubleEntries :: [Double]} | StringCol {name :: String, stringEntries :: [String]} deriving (Eq, Ord)

columnType :: Column -> TypeRep
columnType (IntCol _ xs) = typeOf xs
columnType (DoubleCol _ xs) = typeOf xs
columnType (StringCol _ xs) = typeOf xs

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
			isInt ('-': cs) = isInt' cs
			isInt cs = isInt' cs
			isInt' [] = False
			isInt' [c] = Char.isDigit c 
			isInt' (c:cs) = (isInt' [c]) && (isInt' cs)
			isDouble ('-': cs) = isDouble' cs False
			isDouble cs = isDouble' cs False
			isDouble' [] b = False 
			isDouble' ['.'] True = False
			isDouble' [c] b = (Char.isDigit c) || (c == '.')
			isDouble' ('.':cs) True = False
			isDouble' ('.':cs) False = isDouble' cs True
			isDouble' (c:cs) b = (isDouble' [c] b) && (isDouble' cs b)

entryType :: Entry -> TypeRep
entryType (IntE i) = typeOf i
entryType (DoubleE d) = typeOf d
entryType (StringE s) = typeOf s

