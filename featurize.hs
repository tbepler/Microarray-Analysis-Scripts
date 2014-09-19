
import qualified Data.Set as Set
import qualified Data.Vector as Vector

alphabet = "ACGT"
kmerlens = [1, 2, 3]
test = Vector.fromList "AATCG"

main = putStrLn (unlines $ (header feats):[featureVecToString testfeats]) where
	testfeats = featurize kmerlens feats test
	feats = features alphabet kmerlens len
	len = Vector.length test

header fs = unwords $ header' fs where
	header' [] = [""]
	header' (x:xs) = (featureToString x):(header' xs)

featureToString (i,k) = "["++(show i)++"]"++(Vector.toList k)

featureVecToString fvec = unwords $ map (str) fvec where
	str (f, True) = "1"
	str (f, False) = "0"

features alphabet kmerlens len = concatMap (\k -> [ (i,k) | i<-[1..(len - (Vector.length k) + 1)]] ) kmers where
	kmers = map (Vector.fromList) $ concatMap ( \n -> comb n alphabet ) kmerlens

featurize kmerlens fs s =  map (\f -> (f, Set.member f fSet) ) fs  where
	fSet = foldl (\set e -> Set.insert (feature e s) set) Set.empty $ concatMap ( \n -> [ (i,n) | i<-[1..((Vector.length s) - n + 1)] ] ) kmerlens
	feature (i,n) s = (i, Vector.take n $ Vector.drop (i-1) s)

comb n xs = comb' n xs [[]] where
	comb' n xs ys
		| n <= 0 = ys
		| otherwise = comb' (n-1) xs $ concatMap ( \y -> [ x:y | x<-xs ] ) ys

parselines ls = map (p.words) ls where
	p [x,y] = (Vector.fromList x, y)