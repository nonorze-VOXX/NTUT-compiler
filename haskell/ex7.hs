import Prelude hiding (foldl, foldr, map)

data BTree a
  = Leaf a
  | Branch (BTree a, BTree a)
  deriving (Show)

tree = Branch (Branch (Leaf 1, Leaf 5), Branch (Branch (Leaf 2, Leaf 3), Leaf 4))

hd :: BTree a -> a
hd (Leaf x) = x
hd (Branch (x, _)) = hd x

tl :: BTree a -> BTree a
tl (Leaf x) = error "leaf have no tail"
tl (Branch (x, y)) = aux (Branch (x, y))
  where
    aux (Branch (Leaf _, right)) = right
    aux (Branch (left, right)) = Branch (aux left, right)

mem :: (Eq a) => a -> BTree a -> Bool
mem a (Leaf l) = l == a
mem a (Branch (l, r)) = mem a l || mem a r

rev :: BTree a -> BTree a
rev (Leaf a) = Leaf a
rev (Branch (l, r)) = Branch (rev r, rev l)

map :: (a -> b) -> BTree a -> BTree b
map f (Leaf a) = Leaf (f a)
map f (Branch (l, r)) = Branch (map f l, map f r)

foldl :: (acc -> a -> acc) -> acc -> BTree a -> acc
foldl f acc (Leaf a) = f acc a
foldl f acc (Branch (l, r)) = foldl f (foldl f acc l) r

foldr :: (a -> acc -> acc) -> acc -> BTree a -> acc
foldr f acc (Leaf a) = f a acc
foldr f acc (Branch (l, r)) = foldr f (foldr f acc r) l

seq2list :: BTree a -> [a]
seq2list = foldr (:) []

findOpt :: (Eq a) => a -> BTree a -> Maybe Integer
findOpt x l =
  fst $ foldl aux (Nothing, 0) l
  where
    aux (Nothing, i) s =
      if x == s
        then (Just i, i + 1)
        else (Nothing, i + 1)
    aux (n, i) s = (n, i + 1)

nth :: (Eq a) => BTree a -> Integer -> a
nth s n =
  case fst (foldl aux (Nothing, 0) s) of
    Nothing -> error "index no found"
    Just n' -> n'
  where
    aux (Nothing, i) s' =
      if n == i
        then (Just s', i + 1)
        else (Nothing, i + 1)
    aux (n', i) s = (n', i + 1)
