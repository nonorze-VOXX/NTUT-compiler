squareSum :: (Num a) => [a] -> a
squareSum = foldl (\acc v -> acc + (v ^ 2)) 0

findOpt :: (Eq a) => a -> [a] -> Maybe Integer
findOpt x [] = Nothing
findOpt x (l : ls) =
  if x == l
    then Just 0
    else fmap (+ 1) (findOpt x ls)