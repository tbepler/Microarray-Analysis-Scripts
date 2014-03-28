module DNA where

rvscmpl :: String -> String
rvscmpl sqnc = reverse $ map (cmpl) sqnc

cmpl :: Char -> Char
cmpl 'a' = 't'
cmpl 'c' = 'g'
cmpl 'g' = 'c'
cmpl 't' = 'a'
cmpl 'A' = 'T'
cmpl 'C' = 'G'
cmpl 'G' = 'C'
cmpl 'T' = 'A'
cmpl n = n


