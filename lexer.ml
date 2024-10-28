open A

let rec test string curr =
  let b = { text = string; current = curr; last = curr } in
  try
    let _ = state_2 b in
    Printf.printf "Accepted\n"
  with
  | End_of_file -> Printf.printf "exception End_of_file\n"
  | Failure s ->
    if s = "ok" then (
      Printf.printf "-> %s\n" (String.sub string curr (b.last - curr));
      if b.current = String.length string then Printf.printf "\n"
      else test string b.last)
    else Printf.printf "Error: %s\n" s

let () = test "abaabaaaba" 0
