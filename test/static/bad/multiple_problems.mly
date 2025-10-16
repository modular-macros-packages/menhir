%token <int> INT        (* INT is unused *)
%token PLUS TIMES DIV   (* forget to define MINUS *)
%token LPAREN           (* forget to define RPAREN *)
%token EOL

%left PLUS MINUS
%left TIMES DIV
%nonassoc UMINUS

%start <int> main
%type  <int> exp       (* exp does not exist *)
%type  <int> main      (* redundant *)

%on_error_reduce list(exp)
%on_error_reduce list(exp) (* redundant *)

%%

main:
| e = expr EOL
    { e }

expr:
| i = int              (* int does not exist *)
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = exp (* exp does not exist, again! the problem should be reported only once *)
    { e1 + e2 }
| e1 = expr MINUS e2 = expr (* MINUS has not been properly declared *)
    { e1 - e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
| MINUS e = expr %prec UMINUS (* MINUS has not been properly declared, again *)
    { - e }
