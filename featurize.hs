
alphabet = "ACGT"
kmerlens = [1, 2, 3]

main = print (length xs, xs) where
	xs = comb 3 alphabet

--features alphabet kmerlens len = 

comb n xs = comb' n xs [[]] where
	comb' n xs ys
		| n <= 0 = ys
		| otherwise = comb' (n-1) xs $ concatMap ( \y -> [ x:y | x<-xs ] ) ys

parselines ls = map (p.words) ls where
	p [x,y] = (x, y)