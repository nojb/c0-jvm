%{
open Ast

type stmt =
  | Rdecl of string * exp option
  | Rassign of string * exp
  | Rasnop of string * binop * exp
  | Rreturn of exp

let elab stmt cont =
  match stmt with
  | Rdecl (id, e) ->
      Declare (id, e, cont)
  | Rassign (id, e) ->
      seq (Assign (id, e)) cont
  | Rasnop (id, op, e) ->
      seq (Assign (id, Binop (Ident id, op, e))) cont
  | Rreturn e ->
      seq (Return e) cont

let rec elab_stmts = function
  | [] -> Nop
  | stmt :: stmts -> elab stmt (elab_stmts stmts)
%}

%token INT LEFTPAREN RIGHTPAREN LEFTCURLY RIGHTCURLY SEMICOLON RETURN
%token RETURN LEFTCURLY RIGHTCURLY EOF
%token <int32> INTLIT
%token <string> IDENT
%token EQUAL
%token PLUS MINUS TIMES SLASH PERCENT
%token PLUSEQUAL MINUSEQUAL TIMESEQUAL SLASHEQUAL PERCENTEQUAL

%left PLUS MINUS        /* lowest precedence */
%left TIMES SLASH         /* medium precedence */
%nonassoc UMINUS        /* highest precedence */

%start program             /* the entry point */
%type <string * Ast.stmt> program

%%

program
: INT IDENT LEFTPAREN RIGHTPAREN LEFTCURLY stmts RIGHTCURLY EOF
  { $2, elab_stmts $6 }
;

stmts
: /* empty */                  { [] }
| stmt stmts                   { $1 :: $2 }
;

stmt
: decl SEMICOLON               { $1 }
| simp SEMICOLON               { $1 }
| RETURN exp SEMICOLON         { Rreturn $2 }
;

decl
: INT IDENT                    { Rdecl ($2, None) }
| INT IDENT EQUAL exp          { Rdecl ($2, Some $4) }
;

simp
: lvalue EQUAL exp             { Rassign ($1, $3) }
| lvalue PLUSEQUAL exp         { Rasnop ($1, Add, $3) }
| lvalue MINUSEQUAL exp        { Rasnop ($1, Sub, $3) }
| lvalue TIMESEQUAL exp        { Rasnop ($1, Mul, $3) }
| lvalue SLASHEQUAL exp        { Rasnop ($1, Div, $3) }
| lvalue PERCENTEQUAL exp      { Rasnop ($1, Mod, $3) }
;

lvalue
: IDENT                        { $1 }
| LEFTPAREN lvalue RIGHTPAREN  { $2 }
;

exp
: LEFTPAREN exp RIGHTPAREN     { $2 }
| INTLIT                       { Const $1 }
| IDENT                        { Ident $1 }
| exp PLUS exp                 { Binop ($1, Add, $3) }
| exp MINUS exp                { Binop ($1, Sub, $3) }
| exp TIMES exp                { Binop ($1, Mul, $3) }
| exp SLASH exp                { Binop ($1, Div, $3) }
| exp PERCENT exp              { Binop ($1, Mod, $3) }
| MINUS exp                    { Binop (Const 0l, Sub, $2) }
;
