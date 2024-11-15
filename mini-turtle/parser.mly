
/* Parsing for mini-Turtle */

%{
  open Ast

%}

/* Declaration of tokens */

%token <int> CST
%token EOF
%token NEWLINE
/* To be completed */
%token PLUS MINUS TIMES DIV MOD

%token FORWARD
%token PENDOWN PENUP TURNLEFT TURNRIGHT

%token COLOR
%token BLACK WHITE RED GREEN BLUE

/* Priorities and associativity of tokens */
// %left PLUS MINUS

/* To be completed */

/* Axiom of the grammar */
%start prog

/* Type of values ​​returned by the parser */
%type <Ast.program> prog

%%

/* Production rules of the grammar */

prog:
// | NEWLINE ? b = stmt NEWLINE ? EOF
| NEWLINE ? b = list(stmt) NEWLINE ? EOF
    { { defs = []; main = Sblock b } (* To be modified *) }
    // { { defs = []; main = Sblock [] } (* To be modified *) }
// |EOF
//     { { defs = []; main =  } (* To be modified *) }
;


stmt:
| FORWARD e = expr NEWLINE
    { Sforward e }
| PENUP  NEWLINE
    { Spenup }
| PENDOWN NEWLINE
    { Spendown }
| COLOR c = color NEWLINE
    {Scolor c }
| TURNLEFT NEWLINE
    { Sturn (Econst 90) }
| TURNRIGHT NEWLINE
    { Sturn (Econst (-90)) }
;
expr:
| c = CST
    { Econst c }
| e1 = expr o = binop e2 = expr
    {Ebinop (o,e1,e2)}
    // {Ebinop ( Sub, 0, e1)}
// | c = expr PLUS d = expr
//     { Econst (c+d)}
;
%inline binop:
| PLUS {Add}
| MINUS {Sub}
| TIMES {Mul}
| DIV {Div}
;
color:
| BLACK {Turtle.black}
| WHITE {Turtle.white}
| RED {Turtle.red}
| GREEN {Turtle.green}
| BLUE {Turtle.blue}
;
