
import qualified Data.Ord as Ord
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Vector as Vector

alphabet = "ACGT"
kmerlens = [1, 2, 3]
test = map (Vector.fromList) ["AATCG", "GGTCA", "ACGTT" ]

main = interact $ parse alphabet kmerlens{- putStrLn (toString feats "" testfeats) -} where
	testfeats = map (featurize kmerlens feats) test
	feats = features alphabet kmerlens len
	len = Vector.length $ head test

parse alphabet kmerlens input = toString feats "" $ map (featurize kmerlens feats) sqncs where
	ls = parselines $ lines input
	sqncs = map (\(x,y) -> x) ls
	len = Vector.length $ head sqncs
	feats = features alphabet kmerlens len

toString feats y vecs = unlines $ (header feats y):(map (featVecToString) vecs)

header fs y = unwords $ header' fs y where
	header' [] y = [y]
	header' (x:xs) y = (featureToString x):(header' xs y)

featureToString (i,k) = "["++(show i)++"]"++(Vector.toList k)

featVecToString bvec = unwords $ map (\x -> if x then "1" else "0") bvec

toBinaryVector fvec = map (bin) fvec where
	bin (f, True) = 1
	bin (f, False) = 0

features alphabet kmerlens len = List.sortBy sort $ concatMap (\k -> [ (i,k) | i<-[1..(len - (Vector.length k) + 1)]] ) kmers where
	kmers = map (Vector.fromList) $ concatMap ( \n -> comb n alphabet ) kmerlens
	comp (i,k1) (j,k2)
		| (Vector.length k1) /= (Vector.length k2) = (Vector.length k1) - (Vector.length k2)
		| i /= j = i - j
		| k1 < k2 = -1
		| k1 > k2 = 1
		| otherwise = 0
	sort a b
		| ord < 0 = Ord.LT
		| ord > 0 = Ord.GT
		| otherwise = Ord.EQ
		where ord = comp a b


featurize kmerlens fs s =  map ( contains ) fs  where
	fSet = foldl (\set e -> Set.insert (feature e s) set) Set.empty $ concatMap ( \n -> [ (i,n) | i<-[1..((Vector.length s) - n + 1)] ] ) kmerlens
	contains (i,k) = (feature (i, Vector.length k) s) == (i,k)
	feature (i,n) s = (i, Vector.take n $ Vector.drop (i-1) s)

comb n xs = comb' n xs [[]] where
	comb' n xs ys
		| n <= 0 = ys
		| otherwise = comb' (n-1) xs $ concatMap ( \y -> [ x:y | x<-xs ] ) ys

parselines ls = map (p.words) ls where
	p [x,y] = (Vector.fromList x, y)