type buffer = { text: string; mutable current: int; mutable last: int }
 let next_char b =
 if b.current = String.length b.text then raise End_of_file;
  let c = b.text.[b.current] in
 b.current <- b.current + 1;
c
let rec stateSuck = true
and state_0 b = failwith ("lexical error")
and state_1 b = b.last <- b.current; failwith ("ok")
and state_2 b = let nc = next_char b in 
if 'a' = nc then state_2 b else
if 'b' = nc then state_1 b else
failwith ("lexical error")
let start = state_2
