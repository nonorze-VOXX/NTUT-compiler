
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
let integer = '0' | ['1'-'9'] digit*
let plus = '+'
let minus = '-'
let div = '/'
let comment = "//" [^'\n']* '\n' | "(*" ([^'*'] | '*' [^')'])* "*)"
let repeat = "repeat"
let letter = ['a'-'z' 'A'-'Z']
let ident = (letter | '_') (letter | digit | '_')*

rule token = parse
  | "forward" {FORWARD}
  | space { token lexbuf }
  | integer as i  { CST (int_of_string i) }
  | '\n'+ { NEWLINE }
  | plus {PLUS}
  | minus {MINUS}
  | div {DIV}
  | comment { token lexbuf }
  | "penup" {PENUP}
  | "pendown" {PENDOWN}
  | "turnleft" {TURNLEFT}
  | "turnright" {TURNRIGHT}
  | "color" {COLOR}
  | "black" {BLACK}
  | "white" {WHITE}
  | "red" {RED}
  | "green" {GREEN}
  | "blue" {BLUE}
  | '*' {TIMES}
  | repeat {REPEAT}
  | '{' {LBLOCK}
  | '}' {RBLOCK}
  | '(' {LPAREN}
  | ')' { RPAREN }
  | "def" { DEF }
  | ',' {COMMA}
  | "if" { IF }
  | "else" { ELSE }
  | ident as s { IDENT s }
  | eof { EOF }
  (* | _ { token lexbuf} *)
  | _ { assert false }


