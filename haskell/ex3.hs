import Distribution.FieldGrammar (List)
import Prelude hiding (compare)

palindrome :: String -> Bool
palindrome s = s == reverse s

compare :: String -> String -> Bool
compare [] (y : ys) = True
compare (x : xs) (y : ys) = if x == y then compare xs ys else x < y
compare _ _ = False

factor :: String -> String -> Bool
factor [] _ = True
factor _ [] = False
factor m1 (y : ys) =
  take (length m1) (y : ys) == m1 || factor m1 ys