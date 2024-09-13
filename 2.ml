let rec inner_fibo = function (pre, prepre, n)-> 
  match n with 
  | 0 -> prepre
  | 1 -> pre
  | n ->  inner_fibo(pre+prepre , pre, n-1)

let rec fibo = function n -> inner_fibo(1,0,n)
           
let ans = fibo 4