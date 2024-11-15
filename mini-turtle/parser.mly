
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
%token REPEAT
%token LBLOCK RBLOCK LPAREN RPAREN
%token DEF
%token COMMA
%token <string> IDENT
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
| NEWLINE ? defs = list(def) b = list(stmt) NEWLINE ? EOF
    { { defs = defs; main = Sblock b } (* To be modified *) }
;

def: 
| DEF name=IDENT LPAREN formals = separated_list(COMMA, ident)   RPAREN NEWLINE? LBLOCK NEWLINE b = list(stmt) RBLOCK NEWLINE
    { { name = name; formals = formals; body = Sblock b } }
;
ident:
  id = IDENT { id }
;



params:
| s= IDENT 
    {s}
| s = IDENT COMMA s1=params
    {s}
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
| REPEAT e = expr LBLOCK NEWLINE b = list(stmt) NEWLINE? RBLOCK NEWLINE
    { Srepeat (e, Sblock b) }
| name=IDENT LPAREN args = separated_list(COMMA, expr) RPAREN
    {Scall (name, args)}
| NEWLINE
    { Sblock [] }
;
expr:
| c = CST
    { Econst c }
| e1 = expr o = binop e2 = expr
    {Ebinop (o,e1,e2)}
| n = IDENT
    { Evar n }
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