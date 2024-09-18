{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use splitAt" #-}

split :: [a] -> ([a], [a])
split x = (take halfLen x, drop halfLen x)
  where
    halfLen = length x `div` 2

merge :: (Ord a) => ([a], [a]) -> [a]
merge ([], ys) = ys
merge (xs, []) = xs
merge (x : xs, y : ys) =
  if x > y
    then y : merge (x : xs, ys)
    else x : merge (xs, y : ys)

sort :: (Ord a) => [a] -> [a]
sort xs = case split xs of
  ([], y) -> y
  (x, []) -> x
  (xs', ys) -> merge (sort xs', sort ys)
