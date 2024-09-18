{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
import Data.Bits

factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x - 1)

nb_bit_pos :: Integer -> Integer
nb_bit_pos 0 = 0
nb_bit_pos n = nb_bit_pos (n .>>. 1) + (n .&. 1)
