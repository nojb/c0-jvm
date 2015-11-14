{
open Parser        (* The type token is defined in parser.mli *)
}

rule token = parse
| "//"           { oneline_comment lexbuf }
| [' ' '\t']     { token lexbuf }
| '\n'           { Lexing.new_line lexbuf; token lexbuf }
| "return"       { RETURN }
| "int"          { INT }
| ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']* as lxm { IDENT lxm }
| ['0'-'9']+ as lxm { INTLIT (Int32.of_string lxm) }
| '0' ['x''X'] ['0'-'9''a'-'f''A'-'F']+ as lxm { INTLIT (Int32.of_string lxm) }
| ';'            { SEMICOLON }
| '='            { EQUAL }
| "+="           { PLUSEQUAL }
| '+'            { PLUS }
| "-="           { MINUSEQUAL }
| '-'            { MINUS }
| "*="           { TIMESEQUAL }
| '*'            { TIMES }
| "/="           { SLASHEQUAL }
| '/'            { SLASH }
| "%="           { PERCENTEQUAL }
| '%'            { PERCENT }
| '('            { LEFTPAREN }
| ')'            { RIGHTPAREN }
| '{'            { LEFTCURLY }
| '}'            { RIGHTCURLY }
| eof            { EOF }

and oneline_comment = parse
| '\n'           { Lexing.new_line lexbuf; token lexbuf }
| _              { oneline_comment lexbuf }
| eof            { EOF }
