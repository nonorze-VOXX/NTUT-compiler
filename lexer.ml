open A

let test string =
  let b = { text = string; current = 0; last = 0 } in
  try
    let _ = state_2 b in
    Printf.printf "Accepted\n"
  with
  | End_of_file -> Printf.printf "Accepted\n"
  | Failure s -> Printf.printf "Error: %s\n" s

let () = test "ab"

let () = test "b"

let () = test "aab"

let () = test "aaab"

let () = test "aaaba"
