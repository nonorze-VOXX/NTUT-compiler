
let rec factorial = function
  | 0 -> 1
  | 1-> 1
  | s -> factorial (s-1) * s;;
let ans_a = factorial 5;;

let div2 = function n -> n/2;;
let rec binary = function 
  | 0 -> [0]
  | 1 -> [1]
  | x -> (x mod 2)::(binary (div2(x)))  ;;
let rec get_one_in_list = function (l,n) ->
  match l with
  |[] -> []
  |x::xs -> if x= 1 then n::(get_one_in_list (xs,n+1))
      else get_one_in_list (xs,n+1)
let ans_b = get_one_in_list (binary 8,0);;
                      