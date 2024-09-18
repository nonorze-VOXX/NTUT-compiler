l = [0 .. 10]

rev :: [Integer] -> [Integer]
rev xs = foldl (\acc n -> n : acc) [] xs

my_map :: (Foldable t1) => (t2 -> a) -> t1 t2 -> [a]
my_map f xs = foldr (\n acc -> f (n) : acc) [] xs
