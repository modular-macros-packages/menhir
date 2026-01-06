%token <int> INT
%token PLUS MINUS TIMES DIV
%token LPAREN RPAREN
%token EOL

%start <int> main
%type  <int> expr

%%

expr:
| i = INT
    { i }
| LPAREN e = expr RPAREN
    { e }
| e1 = expr PLUS e2 = expr
    { e1 + e2 }
| e1 = expr MINUS e2 = expr
    { e1 - e2 }
| e1 = expr TIMES e2 = expr
    { e1 * e2 }
| e1 = expr DIV e2 = expr
    { e1 / e2 }
%merge { fun e1 e2 -> Disj(e1, e2) }
| MINUS e = expr
    { - e }

main:
| e = expr EOL
    { e }
