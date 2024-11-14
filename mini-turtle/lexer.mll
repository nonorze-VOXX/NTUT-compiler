
(* Lexical analyser for mini-Turtle *)

{
  open Lexing
  open Parser

  (* raise exception to report a lexical error *)
  exception Lexing_error of string

  (* note : remember to call the Lexing.new_line function
at each carriage return ('\n' character) *)

}
let space = ' ' | '\t'
let digit = ['0'-'9']
let integer = '0' | ['1'-'0'] digit*

rule token = parse
  | "forward" {FORWARD}
  | space { token lexbuf }
  | integer as i  { CST (int_of_string i) }
  (* | newline { new_line lexbuf; token lexbuf } *)
  | '\n' { NEWLINE }
  | eof { EOF }
  | _ { assert false (* To be completed *) }

