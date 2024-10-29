open A
open Printf

let get_token str =
  let rec run_a string curr =
    let b = { text = string; current = curr; last = curr } in
    try
      let _ = start b in
      Printf.printf "Accepted\n"
    with
    | End_of_file -> Printf.printf "exception End_of_file\n"
    | Failure s ->
      if s = "ok" then (
        Printf.printf "-> %s\n" (String.sub string curr (b.last - curr));
        if b.current = String.length string then Printf.printf "\n"
        else run_a string b.last)
      else Printf.printf "Error: %s\n" s
  in
  run_a str 0

(* let () = get_token "abaabaaaba" *)
(* ex6 test *)

let () =
  let input_str = "abbaaab" in
  printf "input_str: %s\n" input_str;
  get_token input_str;
  printf "\n"

let () =
  let input_str = "aba" in
  printf "input_str: %s\n" input_str;
  get_token input_str;
  printf "\n"

let () =
  let input_str = "aac" in
  printf "input_str: %s\n" input_str;
  get_token input_str;
  printf "\n"
