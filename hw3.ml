type ichar = char * int

type regexp = 
  | Epsilon
  | Character of ichar
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp
  
(*type null :  regexp -> bool*)
let rec null (r: regexp ) : bool = 
  match r with
  | Epsilon -> true
  | Character _ -> false
  | Union ( r1, r2 ) -> null r1 || null r2
  | Concat (r1, r2 ) -> null r1 && null r2
  | Star _ -> true


(* ex1 test *)
let () =
  let a = Character ('a', 0) in
  assert (not (null a));
  assert (null (Star a));
  assert (null (Concat (Epsilon, Star Epsilon)));
  assert (null (Union (Epsilon, a)));
  assert (not (null (Concat (a, Star a))))



module Cset = Set.Make(struct type t = ichar let compare = Stdlib.compare end)
    

(*type null :  regexp -> bool*)
let rec null (r: regexp ) : bool = 
  match r with
  | Epsilon -> true
  | Character _ -> false
  | Union ( r1, r2 ) -> null r1 || null r2
  | Concat (r1, r2 ) -> null r1 && null r2
  | Star _ -> true    

let rec first (r :regexp) : Cset.t = 
  match r with
  | Epsilon-> Cset.empty
  | Character c -> Cset.singleton c 
  | Union (r1,r2) -> Cset.union( first(r1)) ( first(r2)) 
  | Concat (r1,r2) -> (match null(r1) with 
      |true ->  Cset.union (first(r1)) (first(r2))
      |false -> first(r1)
    )
  | Star s -> first s
      
      
      
let rec last (r :regexp) : Cset.t =
  match r with
  | Epsilon-> Cset.empty
  | Character c -> Cset.singleton c
  | Union (r1,r2) -> Cset.union( last(r1)) ( last(r2))
  | Concat (r1,r2) -> if null(r2) then 
        Cset.union (last(r2)) (last(r1))
      else last(r2)
  | Star s -> last s
      
(* ex2 test *)
let () =
  let ca = ('a', 0) and cb = ('b', 0) in
  let a = Character ca and b = Character cb in
  let ab = Concat (a, b) in
  let eq = Cset.equal in
  assert (eq (first a) (Cset.singleton ca));
  assert (eq (first ab) (Cset.singleton ca));
  assert (eq (first (Star ab)) (Cset.singleton ca));
  assert (eq (last b) (Cset.singleton cb));
  assert (eq (last ab) (Cset.singleton cb));
  
  assert (Cset.cardinal (first (Union (a, b))) = 2);
  assert (Cset.cardinal (first (Concat (Star a, b))) = 2);
  assert (Cset.cardinal (last (Concat (a, Star b))) = 2)
    


let rec follow (c: ichar) (r :regexp) : Cset.t = 
  match r with
  | Epsilon-> Cset.empty
  | Character rc -> Cset.empty
  | Union (r1,r2) -> Cset.union (follow c r1) (follow c r2)
  | Concat (r1,r2) -> if Cset.equal (Cset.singleton(c)) (last(r1))
      then Cset.union( Cset.union (first r2) (follow c r1) )(follow c r2)
      else follow c r2
  | Star s -> if Cset.mem (c) (last(s))
     then  Cset.union (first(s)) (follow c s)else follow c s


    
(* ex3 test *)
let () =
  let ca = ('a', 0) and cb = ('b', 0) in
  let a = Character ca and b = Character cb in
  let ab = Concat (a, b) in
  assert (Cset.equal (follow ca ab) (Cset.singleton cb));
  assert (Cset.is_empty (follow cb ab));
  let r = Star (Union (a, b)) in
  assert (Cset.cardinal (follow ca r) = 2);
  assert (Cset.cardinal (follow cb r) = 2);
  let r2 = Star (Concat (a, Star b)) in
  assert (Cset.cardinal (follow cb r2) = 2);
  let r3 = Concat (Star a, b) in
  assert (Cset.cardinal (follow ca r3) = 2)


  


type state = Cset.t (* a state is a set of characters *)

(* val next_state : regexp -> state -> char -> state *)
let next_state (r:regexp) (now:state) (c:char) =
  let f (a:ichar) (b:Cset.t) = Cset.union (follow a r) ( b ) 
  in 
  Cset.fold f now Cset.empty


module Cmap = Map.Make(Char) (* dictionary whose keys are characters *)
module Smap = Map.Make(Cset) (* dictionary whose keys are states *)
type autom = {
  start : state;
  trans : state Cmap.t Smap.t 
  (* state dictionary -> (character dictionary -> state) *)
}
let eof = ('#', -1)

let make_dfa r =
  let r = Concat (r, Character eof) in
  (* transitions under construction *)
  let trans = ref Smap.empty in
  let rec transitions q = 
  (* the transitions function constructs all the transitions of the state q,
  if this is the first time q is visited *)
  (* TODO... *)
  true
    (* let getFollow (a:ichar) (b:Cset.t) = Cset.union (follow a r) b in
    let maybeNext = Cset.fold getFollow q Cset.empty in
    let getNextState (c, i) (b:Cset.t) = Cset.union (next_state r q c) b in
    let nState =(Cset.fold getNextState maybeNext Cset.empty) in
    trans := Smap.add q nState !trans *)
    
  in
  let q0 = first r in
  transitions q0;
  { start = q0; trans = !trans } (* fix this syntx error**)


let fprint_state fmt q =
  Cset.iter (fun (c,i) ->
  if c = '#' then Format.fprintf fmt "# " else Format.fprintf fmt "%c%i " c i) q
  let fprint_transition fmt q c q' =
  Format.fprintf fmt "\"%a\" -> \"%a\" [label=\"%c\"];@\n"
  fprint_state q
  fprint_state q'
  c
let fprint_autom fmt a =
  Format.fprintf fmt "digraph A {@\n";
  Format.fprintf fmt " @[\"%a\" [ shape = \"rect\"];@\n" fprint_state a.start;
  Smap.iter
    (fun q t -> Cmap.iter (fun c q' -> fprint_transition fmt q c q') t)
    a.trans;
  Format.fprintf fmt "@]@\n}@."
let save_autom file a =
  let ch = open_out file in
  Format.fprintf (Format.formatter_of_out_channel ch) "%a" fprint_autom a;
  close_out ch

(* (a|b)*a(a|b) *)
let r = Concat (Star (Union (Character ('a', 1), Character ('b', 1))),
          Concat (Character ('a', 2),
            Union (Character ('a', 3), Character ('b', 2))))
let a = make_dfa r
let () = save_autom "autom.dot" a