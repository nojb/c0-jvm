%{
open Ast

let mk desc =
  let startpos = Parsing.symbol_start_pos () in
  let endpos = Parsing.symbol_end_pos () in
  {
    desc;
    loc = (startpos, endpos);
  }

let mkdummy desc =
  {
    desc;
    loc = (Lexing.dummy_pos, Lexing.dummy_pos);
  }

type stmt =
  | Rdecl of string * exp desc option
  | Rassign of string * exp desc
  | Rasnop of string * binop * exp desc
  | Rreturn of exp desc

let elab stmt cont =
  match stmt with
  | Rdecl (id, e) ->
      Declare (id, e, cont)
  | Rassign (id, e) ->
      seq (Assign (id, e)) cont
  | Rasnop (id, op, e) ->
      seq (Assign (id, {e with desc = Binop (mkdummy (Ident id), op, e)})) cont
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

%left PLUS MINUS
%left TIMES SLASH PERCENT
%nonassoc UMINUS

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
| INTLIT                       { mk (Const $1) }
| IDENT                        { mk (Ident $1) }
| exp PLUS exp                 { mk (Binop ($1, Add, $3)) }
| exp MINUS exp                { mk (Binop ($1, Sub, $3)) }
| exp TIMES exp                { mk (Binop ($1, Mul, $3)) }
| exp SLASH exp                { mk (Binop ($1, Div, $3)) }
| exp PERCENT exp              { mk (Binop ($1, Mod, $3)) }
| MINUS exp %prec UMINUS       { mk (Binop (mkdummy (Const 0l), Sub, $2)) }
;
