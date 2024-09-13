(* #!/usr/bin/env nix-shell *)
(*
#!nix-shell --pure -i ocaml -p ocaml
 *)
let explode s = List.init (String.length s) (String.get s)

let rec get_end = function 
  | [] -> ' '
  | x::xs -> if xs= [] then x else get_end xs;;
let rec get_without_end = function 
  | [] -> []
  | x::xs -> if xs= [] then [] else x::get_without_end xs ;;
let start_same_end = function 
  | [] -> true
  | x::xs -> x = get_end xs
               
               
let get_center = function 
  | [] -> []
  | x::xs -> if xs = [] then [] 
      else get_without_end xs
          
let inputStr = explode "madam" 
let rec is_palindrome = function
  | [] -> true
  | x::xs -> 
      match xs with
      | [] -> true
      | xs -> (start_same_end (x::xs)) && is_palindrome (get_center (x::xs))
let tt = is_palindrome inputStr