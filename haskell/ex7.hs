import Data.Void (absurd)

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

mem :: a -> BTree a -> bool
mem = undefined

rev :: BTree a -> BTree a
rev = undefined

map :: (a -> b) -> BTree a -> BTree b
map = undefined

foldl :: (acc -> a -> acc) -> acc -> BTree a -> acc
foldl = undefined

foldr :: (a -> acc -> acc) -> acc -> BTree a -> acc
foldr = undefined

seq2list :: BTree a -> [a]
seq2list = undefined