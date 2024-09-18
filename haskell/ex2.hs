fibo :: Integer -> Integer
fibo 0 = 0
fibo 1 = 1
fibo n = inner_fibo n 0 1
  where
    inner_fibo 0 pre _ = pre
    inner_fibo n' pre prepre = inner_fibo (n' - 1) (pre + prepre) pre