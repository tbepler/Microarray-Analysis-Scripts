--this script combines the replicates from PBMs by taking the median of the fwd and rvs
--replicates

import qualified Probe as Probe

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater)
    where
        lesser = filter (< p) xs
        greater = filter (>= p) xs

select :: Ord a => Int -> [a] -> a
select n xs = (quicksort xs) !! n





