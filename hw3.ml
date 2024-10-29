type ichar = char * int

type regexp =
  | Epsilon
  | Character of ichar
  | Union of regexp * regexp
  | Concat of regexp * regexp
  | Star of regexp

(*type null : regexp -> bool*)
let rec null (r : regexp) : bool =
  match r with
  | Epsilon -> true
  | Character _ -> false
  | Union (r1, r2) -> null r1 || null r2
  | Concat (r1, r2) -> null r1 && null r2
  | Star _ -> true

(* ex1 test *)
let () =
  let a = Character ('a', 0) in
  assert (not (null a));
  assert (null (Star a));
  assert (null (Concat (Epsilon, Star Epsilon)));
  assert (null (Union (Epsilon, a)));
  assert (not (null (Concat (a, Star a))))

module Cset = Set.Make (struct
  type t = ichar

  let compare = Stdlib.compare
end)

open Printf

let print_Cset ns =
  let f (a, b) = a in
  let p = Seq.map f (Cset.to_seq ns) in
  List.iter (printf "%c ") (List.of_seq p);
  printf "\n";
  let f (a, b) = b in
  let p = Seq.map f (Cset.to_seq ns) in
  List.iter (printf "%d ") (List.of_seq p);
  printf "\n"

(*type null : regexp -> bool*)
let rec null (r : regexp) : bool =
  match r with
  | Epsilon -> true
  | Character _ -> false
  | Union (r1, r2) -> null r1 || null r2
  | Concat (r1, r2) -> null r1 && null r2
  | Star _ -> true

let rec first (r : regexp) : Cset.t =
  match r with
  | Epsilon -> Cset.empty
  | Character c -> Cset.singleton c
  | Union (r1, r2) -> Cset.union (first r1) (first r2)
  | Concat (r1, r2) -> (
    match null r1 with
    | true -> Cset.union (first r1) (first r2)
    | false -> first r1)
  | Star s -> first s

let rec last (r : regexp) : Cset.t =
  match r with
  | Epsilon -> Cset.empty
  | Character c -> Cset.singleton c
  | Union (r1, r2) -> Cset.union (last r1) (last r2)
  | Concat (r1, r2) ->
    if null r2 then Cset.union (last r2) (last r1) else last r2
  | Star s -> last s

(* ex2 test *)
let () =
  let ca = ('a', 0)
  and cb = ('b', 0) in
  let a = Character ca
  and b = Character cb in
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

let rec follow (c : ichar) (r : regexp) : Cset.t =
  match r with
  | Epsilon -> Cset.empty
  | Character rc -> Cset.empty
  | Union (r1, r2) -> Cset.union (follow c r1) (follow c r2)
  | Concat (r1, r2) ->
    if Cset.mem c (last r1) then
      Cset.union (Cset.union (first r2) (follow c r1)) (follow c r2)
    else Cset.union (follow c r1) (follow c r2)
  | Star s ->
    if Cset.mem c (last s) then Cset.union (first s) (follow c s)
    else follow c s

(* ex3 test *)
let () =
  let ca = ('a', 0)
  and cb = ('b', 0) in
  let a = Character ca
  and b = Character cb in
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

let cichar (c : char) (i : int) = c

(* val next_state : regexp -> state -> char -> state *)
let next_state (r : regexp) (now : state) (c : char) =
  Cset.fold
    (fun ((c', i) : ichar) (b : Cset.t) ->
      if c' = c then Cset.union (follow (c', i) r) b else b)
    now
    Cset.empty

module Cmap = Map.Make (Char) (* dictionary whose keys are characters *)
module Smap = Map.Make (Cset) (* dictionary whose keys are states *)

type autom =
  { start : state
  ; trans : state Cmap.t Smap.t
        (* state dictionary -> (character dictionary -> state) *)
  }

let eof = ('#', -1)

let make_dfa r =
  let r = Concat (r, Character eof) in
  (* transitions under construction *)
  let trans = ref Smap.empty in
  let rec transitions q =
    let is_state_in now _ (all : bool) = now = q || all in
    let is_in = Smap.fold is_state_in !trans false in
    if is_in then ignore ()
    else
      (* the transitions function constructs all the transitions of the state q,
         if this is the first time q is visited *)
      let maybeNext = q in
      let getNextState (c, i) (b : state Cmap.t) =
        Cmap.add c (next_state r q c) b
      in
      let nState = Cset.fold getNextState maybeNext Cmap.empty in
      trans := Smap.add q nState !trans;

      let callNextTransition k v = transitions v in

      Cmap.iter callNextTransition nState
  in
  let q0 = first r in
  transitions q0;
  { start = q0; trans = !trans }

let fprint_state fmt q =
  Cset.iter
    (fun (c, i) ->
      if c = '#' then Format.fprintf fmt "# "
      else Format.fprintf fmt "%c%i " c i)
    q

let fprint_transition fmt q c q' =
  Format.fprintf
    fmt
    "\"%a\" -> \"%a\" [label=\"%c\"];@\n"
    fprint_state
    q
    fprint_state
    q'
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
let r =
  Concat
    ( Star (Union (Character ('a', 1), Character ('b', 1)))
    , Concat (Character ('a', 2), Union (Character ('a', 3), Character ('b', 2)))
    )

(* ex4 *)
let a = make_dfa r

let () = save_autom "autom.dot" a

let first_char s = if String.length s > 0 then Some (String.get s 0) else None

let without_first_char s =
  if String.length s > 1 then Some (String.sub s 1 (String.length s - 1))
  else None

(* val recognize : autom -> string -> bool *)
let recognize (a : autom) (s : string) =
  let s = String.cat s "#" in
  let rec single now_state ss =
    match first_char ss with
    | Some c ->
      let nextStateSet = Smap.find now_state a.trans in
      if Cmap.mem c nextStateSet then
        let nextState = Cmap.find c nextStateSet in
        if c == '#' then Some now_state
        else
          match without_first_char ss with
          | None -> None
          | Some s -> single nextState s
      else None
    | None -> Some now_state
  in
  let init_state = a.start in
  match single init_state s with
  | Some _ -> true
  | None -> false

(* ex5 *)
let () = assert (recognize a "aa")

let () = assert (recognize a "ab")

let () = assert (recognize a "abababaab")

let () = assert (recognize a "babababab")

let () = assert (recognize a (String.make 1000 'b' ^ "ab"))

let () = assert (not (recognize a "a"))

let () = assert (not (recognize a "b"))

let () = assert (not (recognize a "ba"))

let () = assert (not (recognize a "aba"))

let () = assert (not (recognize a "abababaaba"))

let r =
  Star
    (Union
       ( Star (Character ('a', 1))
       , Concat
           ( Character ('b', 1)
           , Concat (Star (Character ('a', 2)), Character ('b', 2)) ) ))

let a = make_dfa r

let () =
  printf "\n";
  save_autom "autom2.dot" a

let () = assert (recognize a "")

let () = assert (recognize a "bb")

let () = assert (recognize a "aaa")

let () = assert (recognize a "aaabbaaababaaa")

let () = assert (recognize a "bbbbbbbbbbbbbb")

let () = assert (recognize a "bbbbabbbbabbbabbb")

let () = assert (not (recognize a "b"))

let () = assert (not (recognize a "ba"))

let () = assert (not (recognize a "ab"))

let () = assert (not (recognize a "aaabbaaaaabaaa"))

let () = assert (not (recognize a "bbbbbbbbbbbbb"))

let () = assert (not (recognize a "bbbbabbbbabbbabbbb"))
(* ex6 *)

let r3 = Concat (Star (Character ('a', 1)), Character ('b', 1))

let a = make_dfa r3

let () = save_autom "autom3.dot" a

let find_index trans s =
  Smap.fold
    (fun k v (index, finded) ->
      if finded then (index, finded)
      else if k = s then (index, true)
      else (index + 1, false))
    trans
    (0, false)

let get_index_of_state (s : state) (a : autom) =
  match find_index a.trans s with
  | index, true -> index
  | _, false -> -1

let is_final_state s = Cset.mem eof s

let no_next_trans s = Cset.is_empty s

let rec autom_to_fprintf fmt a =
  Format.fprintf
    fmt
    "type buffer = { text: string; mutable current: int; mutable last: int }\n";
  Format.fprintf fmt " let next_char b =\n ";
  Format.fprintf
    fmt
    "if b.current = String.length b.text then raise End_of_file;\n ";
  Format.fprintf fmt " let c = b.text.[b.current] in\n ";
  Format.fprintf fmt "b.current <- b.current + 1;\n";
  Format.fprintf fmt "c\n";
  Format.fprintf fmt "let rec stateSuck = true\n";
  ignore
    (Smap.fold
       (fun q v index ->
         Format.fprintf fmt "and state_%d b = " (get_index_of_state q a);
         if no_next_trans q then
           Format.fprintf fmt "failwith (\"lexical error\")\n"
         else if is_final_state q then
           Format.fprintf fmt "b.last <- b.current; failwith (\"ok\")\n"
         else (
           Format.fprintf fmt "let nc = next_char b in \n";
           Cmap.iter
             (fun k v ->
               Format.fprintf
                 fmt
                 "if '%c' = nc then state_%d b else\n"
                 k
                 (get_index_of_state v a))
             v;
           Format.fprintf fmt "failwith (\"lexical error\")\n");
         index + 1)
       a.trans
       0)

let generate file_name (a : autom) =
  let ch = open_out file_name in
  Format.fprintf (Format.formatter_of_out_channel ch) "%a" autom_to_fprintf a;
  close_out ch

let () = generate "a.ml" a
