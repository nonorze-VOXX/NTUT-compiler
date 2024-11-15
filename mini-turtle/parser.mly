
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